namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module PickupDispatcherModule =

    type PickupType =
        | Health
    
    type [<StructuralEquality; NoComparison>] PickupModel =
        { PickupType : PickupType
          PickupSheet : Image AssetTag
          PickupSheetPositionM : Vector2i }

        static member health =
            { PickupType = Health
              PickupSheet = Assets.PickupSheetImage
              PickupSheetPositionM = Vector2i.Zero }

    type Entity with
        member this.GetPickupModel = this.GetModel<PickupModel>
        member this.SetPickupModel = this.SetModel<PickupModel>
        member this.PickupModel = this.Model<PickupModel> ()

    type PickupDispatcher () =
        inherit EntityDispatcher<PickupModel, unit, unit> (PickupModel.health)

        static let getSpriteInsetOpt model =
            let spriteOffset =
                Vector2
                    (Constants.Layout.TileSize.X * single model.PickupSheetPositionM.X,
                     Constants.Layout.TileSize.Y * single model.PickupSheetPositionM.Y)
            let spriteInset =
                Vector4
                    (spriteOffset.X,
                     spriteOffset.Y,
                     Constants.Layout.TileSize.X,
                     Constants.Layout.TileSize.Y)
            Some spriteInset
        
        override this.View (model, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                [Render (transform.Depth, transform.Position.Y, AssetTag.generalize model.PickupSheet,
                     SpriteDescriptor
                       { Transform = transform
                         Offset = Vector2.Zero
                         InsetOpt = getSpriteInsetOpt model
                         Image = model.PickupSheet
                         Color = Color.White
                         Glow = Color.Zero
                         Flip = FlipNone })]
            else []