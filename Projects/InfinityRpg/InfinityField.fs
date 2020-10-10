namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module FieldDispatcherModule =

    type [<StructuralEquality; NoComparison>] FieldModel =
        { FieldMapNp : FieldMap }

        static member initial =
            let DefaultRand = Rand.make ()
            let DefaultSizeM = Vector2i (4, 4)
            let DefaultPathEdgesM = [(Vector2i (1, 1), Vector2i (2, 2))]
            let DefaultFieldMap = fst (FieldMap.make Assets.FieldTileSheetImage (Vector2i.Zero) DefaultSizeM DefaultPathEdgesM DefaultRand)
            { FieldMapNp = DefaultFieldMap }
    
    type Entity with
    
        member this.GetFieldModel = this.GetModel<FieldModel>
        member this.SetFieldModel = this.SetModel<FieldModel>
        member this.FieldModel = this.Model<FieldModel> ()

    type FieldDispatcher () =
        inherit EntityDispatcher<FieldModel, unit, unit> (FieldModel.initial)

        static let getTileInsetOpt (tileSheetPositionM : Vector2i) =
            let tileOffset = vmtovf tileSheetPositionM
            let tileInset =
                Vector4
                    (tileOffset.X,
                     tileOffset.Y,
                     Constants.Layout.TileSize.X,
                     Constants.Layout.TileSize.Y)
            Some tileInset

        static let viewBoundsToMapUnits (viewBounds : Vector4) =
            let right = int viewBounds.X + int viewBounds.Z
            let top = int viewBounds.Y + int viewBounds.W
            Vector4i
                (itom (int viewBounds.X),
                 itom (int viewBounds.Y),
                 (if isSnapped right then (itom right) - 1 else itom right),
                 (if isSnapped top then (itom top) - 1 else itom top))

        static let tilePositionInView (tilePositionM : Vector2i) (mInViewBounds : Vector4i) =
            tilePositionM.X >= mInViewBounds.X &&
            tilePositionM.Y >= mInViewBounds.Y &&
            tilePositionM.X <= mInViewBounds.X + mInViewBounds.Z &&
            tilePositionM.Y <= mInViewBounds.Y + mInViewBounds.W
        
        static member Properties =
            [define Entity.Omnipresent true]

        override this.Initializers (model, _) =
            [Entity.Size <== model --> fun (model : FieldModel) -> vmtovf model.FieldMapNp.FieldSizeM]
        
        override this.View (model, field, world) =

            let fieldTransform = field.GetTransform world
            let tileTransform = { fieldTransform with RefCount = 0; Size = Constants.Layout.TileSize }
            let absolute = field.GetAbsolute world

            let bounds =
                v4BoundsOverflow
                    fieldTransform.Position
                    (Vector2.Multiply (Constants.Layout.TileSize, Constants.Layout.TileSheetSize))
                    (field.GetOverflow world)

            if World.isBoundsInView absolute bounds world then
                let fieldMap = model.FieldMapNp
                let image = fieldMap.FieldTileSheet
                let mInViewBounds = World.getViewBounds absolute world |> viewBoundsToMapUnits
                let tiles = fieldMap.FieldTiles
                let sprites =
                    Map.foldBack
                        (fun tilePositionM tile sprites ->
                            if tilePositionInView tilePositionM mInViewBounds then
                                let tilePosition = vmtovf tilePositionM // NOTE: field position assumed at origin
                                let tileInsetOpt = getTileInsetOpt tile.TileSheetPositionM
                                let tileTransform = { tileTransform with Position = tilePosition }
                                let sprite =
                                    { Transform = tileTransform
                                      Offset = Vector2.Zero
                                      InsetOpt = tileInsetOpt
                                      Image = image
                                      Color = Color.White
                                      Glow = Color.Zero
                                      Flip = FlipNone }
                                sprite :: sprites
                            else sprites)
                        tiles [] |>
                    Array.ofList

                [Render (fieldTransform.Depth, fieldTransform.Position.Y, AssetTag.generalize image, SpritesDescriptor sprites)]
            else []

        override this.GetQuickSize (field, world) =
            vmtovf ((field.GetFieldModel world).FieldMapNp).FieldSizeM