namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module CharacterDispatcherModule =

    type [<StructuralEquality; NoComparison>] CharacterModel =
        { 
          // gameplay logic data
        
          Index : CharacterIndex
          Turn : Turn
          CharacterState : CharacterState
          PositionM : Vector2i
          
          // "puppet show" data
          
          CharacterActivityState : CharacterActivityState
          CharacterAnimationState : CharacterAnimationState
          CharacterAnimationSheet : Image AssetTag
          Position : Vector2 }

        static member initial =
            let characterAnimationState =
                { StartTime = 0L
                  AnimationType = CharacterAnimationFacing
                  Direction = Upward }
            { Index = PlayerIndex
              Turn = NoTurn
              CharacterState = CharacterState.empty
              PositionM = Vector2i.Zero
              CharacterActivityState = NoActivity
              CharacterAnimationState = characterAnimationState
              CharacterAnimationSheet = Assets.PlayerImage
              Position = Vector2.Zero }

        static member updatePosition newValue (model : CharacterModel) =
            { model with Position = newValue }

        static member updatePositionM newValue (model : CharacterModel) =
            { model with PositionM = newValue }

        static member updateCharacterActivityState newValue (model : CharacterModel) =
            { model with CharacterActivityState = newValue }

        static member updateCharacterState newValue (model : CharacterModel) =
            { model with CharacterState = newValue }

        static member updateCharacterAnimationState newValue (model : CharacterModel) =
            { model with CharacterAnimationState = newValue }

        static member updateTurn newValue (model : CharacterModel) =
            { model with Turn = newValue }
    
    type Entity with
        member this.GetCharacterModel = this.GetModel<CharacterModel>
        member this.SetCharacterModel = this.SetModel<CharacterModel>
        member this.CharacterModel = this.Model<CharacterModel> ()
    
    type CharacterDispatcher () =
        inherit EntityDispatcher<CharacterModel, unit, unit> (CharacterModel.initial)

        static let getSpriteInsetOpt (entity : Entity) world =
            let animationState = (entity.GetCharacterModel world).CharacterAnimationState
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
                     spriteOffset.X + Constants.Layout.TileSize.X,
                     spriteOffset.Y + Constants.Layout.TileSize.Y)
            Some spriteInset

        static member Properties =
            [define Entity.Depth Constants.Layout.CharacterDepth
             define Entity.PublishChanges true
             define Entity.Omnipresent true]

        override this.Initializers (model, _) =
            [Entity.Position <== model --> fun (model : CharacterModel) -> model.Position]
        
        override this.Actualize (entity, world) =
            if entity.GetInView world then
                let transform =
                    { RefCount = 0
                      Position = entity.GetPosition world
                      Size = entity.GetSize world
                      Rotation = entity.GetRotation world
                      Depth = entity.GetDepth world 
                      Flags = entity.GetFlags world }
                World.enqueueRenderMessage
                    (LayeredDescriptorMessage
                        { Depth = entity.GetDepth world
                          AssetTag = (entity.GetCharacterModel world).CharacterAnimationSheet
                          PositionY = (entity.GetPosition world).Y
                          RenderDescriptor =
                            SpriteDescriptor
                                { Transform = transform
                                  Offset = Vector2.Zero
                                  InsetOpt = getSpriteInsetOpt entity world
                                  Image = (entity.GetCharacterModel world).CharacterAnimationSheet
                                  Color = Vector4.One
                                  Glow = Vector4.Zero
                                  Flip = FlipNone }})
                    world
            else world
