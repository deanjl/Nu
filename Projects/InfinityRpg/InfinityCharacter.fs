﻿namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module CharacterDispatcherModule =

    type [<StructuralEquality; NoComparison>] CharacterModel =
        { Index : CharacterIndex
          Turn : Turn
          CharacterState : CharacterState
          TurnStatus : TurnStatus
          CharacterActivityState : CharacterActivityState
          CharacterAnimationState : CharacterAnimationState
          CharacterAnimationSheet : Image AssetTag
          Position : Vector2 }

        static member initial =
            { Index = PlayerIndex
              Turn = NoTurn
              CharacterState = CharacterState.empty
              TurnStatus = Idle
              CharacterActivityState = NoActivity
              CharacterAnimationState = CharacterAnimationState.initial
              CharacterAnimationSheet = Assets.PlayerImage
              Position = Vector2.Zero }

        static member updateTurn newValue (model : CharacterModel) =
            { model with Turn = newValue }

        static member updateCharacterState newValue (model : CharacterModel) =
            { model with CharacterState = newValue }
        
        static member updateTurnStatus newValue (model : CharacterModel) =
            { model with TurnStatus = newValue }
        
        static member updateCharacterActivityState newValue (model : CharacterModel) =
            { model with CharacterActivityState = newValue }

        static member updateCharacterAnimationState newValue (model : CharacterModel) =
            { model with CharacterAnimationState = newValue }

        static member updatePosition newValue (model : CharacterModel) =
            { model with Position = newValue }
        
        static member makePlayer positionM =
            let characterState = { CharacterState.empty with HitPoints = 30; ControlType = PlayerControlled }
            { CharacterModel.initial with
                CharacterState = characterState
                Position = vmtovf positionM }

        static member makeEnemy index positionM =
            let characterState = { CharacterState.empty with HitPoints = 10; ControlType = Chaos }
            { CharacterModel.initial with
                Index = index
                CharacterState = characterState
                CharacterAnimationSheet = Assets.GoopyImage
                Position = vmtovf positionM }
    
    type Entity with
        member this.GetCharacterModel = this.GetModel<CharacterModel>
        member this.SetCharacterModel = this.SetModel<CharacterModel>
        member this.CharacterModel = this.Model<CharacterModel> ()
    
    type CharacterDispatcher () =
        inherit EntityDispatcher<CharacterModel, unit, unit> (CharacterModel.initial)

        static let getSpriteInsetOpt model world =
            let animationState = model.CharacterAnimationState
            let animationFrames =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> 2
                | CharacterAnimationActing -> 2
                | CharacterAnimationDefending -> 1
                | CharacterAnimationSpecial -> 1
                | CharacterAnimationSlain -> 1
            let animationOffsetM =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> Vector2i (0, 0)
                | CharacterAnimationActing -> Vector2i (0, 2)
                | CharacterAnimationDefending -> Vector2i (4, 0)
                | CharacterAnimationSpecial -> Vector2i (6, 0)
                | CharacterAnimationSlain -> Vector2i (4, 2)
            let animationDelay =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> Constants.InfinityRpg.CharacterAnimationFacingDelay
                | CharacterAnimationActing -> Constants.InfinityRpg.CharacterAnimationActingDelay
                | CharacterAnimationDefending -> 1L // doesn't matter - no animation frames
                | CharacterAnimationSpecial -> 1L // doesn't matter - no animation frames
                | CharacterAnimationSlain -> 1L // doesn't matter - no animation frames
            let directionCoordsOffset =
                match animationState.Direction with
                | Upward -> Vector2i (0, 0)
                | Rightward -> Vector2i (animationFrames, 0)
                | Downward -> Vector2i (0, 1)
                | Leftward -> Vector2i (animationFrames, 1)
            let animatedXOffsetM =
                Math.Abs (World.getTickTime world - animationState.StartTime) /
                animationDelay % int64 animationFrames |>
                int
            let animatedOffsetM = Vector2i (animatedXOffsetM, 0)
            let spriteCoordsinates = animationOffsetM + directionCoordsOffset + animatedOffsetM
            let spriteOffset =
                Vector2
                    (Constants.Layout.TileSize.X * single spriteCoordsinates.X,
                     Constants.Layout.TileSize.Y * single spriteCoordsinates.Y)
            let spriteInset =
                Vector4
                    (spriteOffset.X,
                     spriteOffset.Y,
                     Constants.Layout.TileSize.X,
                     Constants.Layout.TileSize.Y)
            Some spriteInset

        static member Properties =
            [define Entity.Depth Constants.Layout.CharacterDepth
             define Entity.PublishChanges true
             define Entity.Omnipresent true]

        override this.Initializers (model, _) =
            [Entity.Position <== model --> fun (model : CharacterModel) -> model.Position]
        
        override this.View (model, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                let transform = { transform with RefCount = 0 }
                [Render (transform.Depth, transform.Position.Y, AssetTag.generalize model.CharacterAnimationSheet,
                     SpriteDescriptor
                       { Transform = transform
                         Offset = Vector2.Zero
                         InsetOpt = getSpriteInsetOpt model world
                         Image = model.CharacterAnimationSheet
                         Color = Color.White
                         Glow = Color.Zero
                         Flip = FlipNone })]
            else []
