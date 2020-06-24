namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module PlayerDispatcherModule =

    type PlayerDispatcher () =
        inherit CharacterDispatcher ()

        static member Facets =
            [typeof<CharacterCameraFacet>]

        static member Properties =
            [define Entity.CharacterState { CharacterState.empty with HitPoints = 30; ControlType = PlayerControlled }]

        override this.Register (entity, world) =
            let world = base.Register (entity, world)
            let characterState = { CharacterState.empty with HitPoints = 30; ControlType = PlayerControlled }
            entity.SetCharacterModel { CharacterModel.initial with CharacterState = characterState } world