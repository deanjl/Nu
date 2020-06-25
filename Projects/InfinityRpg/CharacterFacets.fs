namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

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