namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module FieldDispatcherModule =

    type Entity with
    
        member this.GetFieldMapNp = this.Get Property? FieldMapNp
        member this.SetFieldMapNp = this.Set Property? FieldMapNp
        member this.FieldMapNp = lens<FieldMap> Property? FieldMapNp this.GetFieldMapNp this.SetFieldMapNp this

    type FieldDispatcher () =
        inherit EntityDispatcher ()

        static let DefaultRand = Rand.make ()
        static let DefaultSizeM = Vector2i (4, 4)
        static let DefaultPathEdgesM = [(Vector2i (1, 1), Vector2i (2, 2))]
        static let DefaultFieldMap = fst (FieldMap.make Assets.FieldTileSheetImage (Vector2i.Zero) DefaultSizeM DefaultPathEdgesM DefaultRand)

        static let getTileInsetOpt (tileSheetPositionM : Vector2i) =
            let tileOffset = vmtovf tileSheetPositionM
            let tileInset =
                Vector4
                    (tileOffset.X,
                     tileOffset.Y,
                     tileOffset.X + Constants.Layout.TileSize.X,
                     tileOffset.Y + Constants.Layout.TileSize.Y)
            Some tileInset

        static let viewBoundsToMapUnits (viewBounds : Vector4) =
            let right = int viewBounds.Z
            let top = int viewBounds.W
            Vector4i
                (itom (int viewBounds.X),
                 itom (int viewBounds.Y),
                 (if isSnapped right then (itom right) - 1 else itom right),
                 (if isSnapped top then (itom top) - 1 else itom top))

        static let tilePositionInView (tilePositionM : Vector2i) (mInViewBounds : Vector4i) =
            tilePositionM.X >= mInViewBounds.X &&
            tilePositionM.Y >= mInViewBounds.Y &&
            tilePositionM.X <= mInViewBounds.Z &&
            tilePositionM.Y <= mInViewBounds.W
        
        static member Properties =
            [define Entity.Omnipresent true
             define Entity.FieldMapNp DefaultFieldMap]

        override this.Actualize (field, world) =

            let absolute =
                field.GetAbsolute world

            let bounds =
                Math.makeBoundsOverflow
                    (field.GetPosition world)
                    (Vector2.Multiply (Constants.Layout.TileSize, Constants.Layout.TileSheetSize))
                    (field.GetOverflow world)

            if World.isBoundsInView absolute bounds world then
                let fieldMap = field.GetFieldMapNp world
                let image = fieldMap.FieldTileSheet
                let mInViewBounds = World.getViewBounds absolute world |> viewBoundsToMapUnits
                let tiles = fieldMap.FieldTiles
                let sprites =
                    Map.foldBack
                        (fun tilePositionM tile sprites ->
                            if tilePositionInView tilePositionM mInViewBounds then
                                let tilePosition = vmtovf tilePositionM // NOTE: field position assumed at origin
                                let tileInsetOpt = getTileInsetOpt tile.TileSheetPositionM
                                let tileTransform =
                                    { Position = tilePosition
                                      Size = Constants.Layout.TileSize
                                      Rotation = 0.0f // NOTE: rotation assumed zero
                                      Depth = field.GetDepth world
                                      Flags = field.GetFlags world }
                                let sprite =
                                    { Transform = tileTransform
                                      Offset = Vector2.Zero
                                      InsetOpt = tileInsetOpt
                                      Image = image
                                      Color = Vector4.One
                                      Glow = Vector4.Zero
                                      Flip = FlipNone }
                                sprite :: sprites
                            else sprites)
                        tiles [] |>
                    Array.ofList

                World.enqueueRenderMessage
                    (LayeredDescriptorMessage
                        { Depth = field.GetDepth world
                          AssetTag = image
                          PositionY = (field.GetPosition world).Y
                          RenderDescriptor = SpritesDescriptor sprites })

                    world
            else world

        override this.GetQuickSize (field, world) =
            vmtovf (field.GetFieldMapNp world).FieldSizeM