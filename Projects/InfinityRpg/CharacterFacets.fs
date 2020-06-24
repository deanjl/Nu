﻿namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module CharacterStateFacetModule =

    type Entity with
    
        member this.GetCharacterActivityState = this.Get Property? CharacterActivityState
        member this.SetCharacterActivityState = this.Set Property? CharacterActivityState
        member this.CharacterActivityState = lens<CharacterActivityState> Property? CharacterActivityState this.GetCharacterActivityState this.SetCharacterActivityState this
        member this.GetCharacterState = this.Get Property? CharacterState
        member this.SetCharacterState = this.Set Property? CharacterState
        member this.CharacterState = lens<CharacterState> Property? CharacterState this.GetCharacterState this.SetCharacterState this

    type CharacterStateFacet () =
        inherit Facet ()

        static member Properties =
            [define Entity.CharacterActivityState NoActivity
             define Entity.CharacterState CharacterState.empty]

[<AutoOpen>]
module CharacterAnimationFacetModule =

    type Entity with
    
        member this.GetCharacterAnimationState = this.Get Property? CharacterAnimationState
        member this.SetCharacterAnimationState = this.Set Property? CharacterAnimationState
        member this.CharacterAnimationState = lens<CharacterAnimationState> Property? CharacterAnimationState this.GetCharacterAnimationState this.SetCharacterAnimationState this
        member this.GetCharacterAnimationSheet = this.Get Property? CharacterAnimationSheet
        member this.SetCharacterAnimationSheet = this.Set Property? CharacterAnimationSheet
        member this.CharacterAnimationSheet = lens<Image AssetTag> Property? CharacterAnimationSheet this.GetCharacterAnimationSheet this.SetCharacterAnimationSheet this

    type CharacterAnimationFacet () =
        inherit Facet ()
        
        static let getSpriteInsetOpt (entity : Entity) world =
            let animationState = entity.GetCharacterAnimationState world
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
            [define Entity.CharacterAnimationState
                    { StartTime = 0L
                      AnimationType = CharacterAnimationFacing
                      Direction = Upward }
             define Entity.CharacterAnimationSheet Assets.PlayerImage]

        override this.Actualize (entity, world) =
            if entity.GetInView world then
                let transform =
                    { Position = entity.GetPosition world
                      Size = entity.GetSize world
                      Rotation = entity.GetRotation world
                      Depth = entity.GetDepth world 
                      Flags = entity.GetFlags world }
                World.enqueueRenderMessage
                    (LayeredDescriptorMessage
                        { Depth = entity.GetDepth world
                          AssetTag = entity.GetCharacterAnimationSheet world
                          PositionY = (entity.GetPosition world).Y
                          RenderDescriptor =
                            SpriteDescriptor
                                { Transform = transform
                                  Offset = Vector2.Zero
                                  InsetOpt = getSpriteInsetOpt entity world
                                  Image = entity.GetCharacterAnimationSheet world
                                  Color = Vector4.One
                                  Glow = Vector4.Zero
                                  Flip = FlipNone }})
                    world
            else world

[<AutoOpen>]
module CharacterCameraFacetModule =

    type CharacterCameraFacet () =
        inherit Facet ()

        static let handlePostUpdate evt world =
            let character = evt.Subscriber : Entity
            let eyeCenter = character.GetPosition world + character.GetSize world * 0.5f
            let eyeCenter =
                if Simulants.Field.Exists world then
                    let eyeSize = World.getEyeSize world
                    let eyeCornerNegative = eyeCenter - eyeSize * 0.5f
                    let eyeCornerPositive = eyeCenter + eyeSize * 0.5f
                    let fieldCornerNegative = Simulants.Field.GetPosition world
                    let fieldCornerPositive = Simulants.Field.GetPosition world + Simulants.Field.GetSize world
                    let fieldBoundsNegative = fieldCornerNegative + eyeSize * 0.5f
                    let fieldBoundsPositive = fieldCornerPositive - eyeSize * 0.5f
                    let eyeCenterX =
                        if eyeCornerNegative.X < fieldCornerNegative.X then fieldBoundsNegative.X
                        elif eyeCornerPositive.X > fieldCornerPositive.X then fieldBoundsPositive.X
                        else eyeCenter.X
                    let eyeCenterY =
                        if eyeCornerNegative.Y < fieldCornerNegative.Y then fieldBoundsNegative.Y
                        elif eyeCornerPositive.Y > fieldCornerPositive.Y then fieldBoundsPositive.Y
                        else eyeCenter.Y
                    Vector2 (eyeCenterX, eyeCenterY)
                else eyeCenter
            World.setEyeCenter eyeCenter world

        override this.Register (entity, world) =
            Stream.monitor handlePostUpdate entity (Stream.make entity.PostUpdateEvent) world