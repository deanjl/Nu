﻿namespace InfinityRpg
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
          PickupSheetPositionM : Vector2i
          Position : Vector2 }

        static member initial =
            { PickupType = Health
              PickupSheet = Assets.PickupSheetImage
              PickupSheetPositionM = Vector2i.Zero
              Position = Vector2.Zero }

        static member makeHealth positionM =
            { PickupModel.initial with Position = vmtovf positionM }

    type Entity with
        member this.GetPickupModel = this.GetModel<PickupModel>
        member this.SetPickupModel = this.SetModel<PickupModel>
        member this.PickupModel = this.Model<PickupModel> ()

    type PickupDispatcher () =
        inherit EntityDispatcher<PickupModel, unit, unit> (PickupModel.initial)

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
        
        static member Properties =
            [define Entity.Depth Constants.Layout.PickupDepth
             define Entity.PublishChanges true
             define Entity.Omnipresent true]
        
        override this.Initializers (model, _) =
            [Entity.Position <== model --> fun (model : PickupModel) -> model.Position]
        
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