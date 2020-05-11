﻿namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module AvatarDispatcherModule =

    type AvatarMessage =
        | Update

    type Entity with
        
        member this.GetAvatarModel = this.GetModel<AvatarModel>
        member this.SetAvatarModel = this.SetModel<AvatarModel>
        member this.AvatarModel = this.Model<AvatarModel> ()

    type AvatarDispatcher () =
        inherit EntityDispatcher<AvatarModel, AvatarMessage, unit>
            (AvatarModel.make Assets.FinnAnimationSheet Downward (Math.makeBounds (v2 -200.0f -200.0f) Constants.Gameplay.CharacterSize))

        static let getSpriteInset (avatar : Entity) world =
            let model = avatar.GetAvatarModel world
            let index = AvatarModel.getAnimationIndex (World.getTickTime world) model
            let offset = v2 (single index.X) (single index.Y) * Constants.Gameplay.CharacterSize
            let inset = Math.makeBounds offset Constants.Gameplay.CharacterSize
            inset

        static member Facets =
            [typeof<RigidBodyFacet>]

        static member Properties =
            [define Entity.AnimationDelay 6L
             define Entity.CelSize Constants.Gameplay.CharacterSize
             define Entity.CelRun 8
             define Entity.FixedRotation true
             define Entity.GravityScale 3.0f
             define Entity.CollisionBody (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = v2Zero })]

        override this.Channel (_, entity, _) =
            [entity.UpdateEvent => [msg Update]]

        override this.Initializers (model, entity, world) =
            [entity.Bounds == (model.Get world).BoundsOriginal
             entity.LinearDamping == 8.0f
             entity.GravityScale == 0.0f]

        override this.Message (model, message, entity, world) =
            let time = World.getTickTime world
            match message with
            | Update ->

                // update animation
                let velocity = World.getBodyLinearVelocity (entity.GetPhysicsId world) world
                let direction = Direction.fromVector2 velocity
                let speed = velocity.Length
                let model =
                    if speed > 10.0f then
                        if direction <> model.Direction || model.AnimationCycle = IdleCycle then
                            let model = AvatarModel.updateDirection (constant direction) model
                            AvatarModel.animate time WalkCycle model
                        else model
                    else AvatarModel.animate time IdleCycle model

                // update bounds
                let model = AvatarModel.updateBounds (constant (entity.GetBounds world)) model
                just model

        override this.Actualize (entity, world) =
            let model = entity.GetAvatarModel world
            if entity.GetVisibleLayered world && entity.GetInView world then
                World.enqueueRenderMessage
                    (RenderDescriptorMessage
                        (LayerableDescriptor
                            { Depth = entity.GetDepth world
                              PositionY = (entity.GetPosition world).Y
                              AssetTag = model.AnimationSheet
                              LayeredDescriptor =
                              SpriteDescriptor
                                { Position = entity.GetPosition world
                                  Size = entity.GetSize world
                                  Rotation = entity.GetRotation world
                                  Offset = Vector2.Zero
                                  ViewType = entity.GetViewType world
                                  InsetOpt = Some (getSpriteInset entity world)
                                  Image = model.AnimationSheet
                                  Color = v4One
                                  Glow = v4Zero
                                  Flip = FlipNone }}))
                    world
            else world