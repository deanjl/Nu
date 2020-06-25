namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module EnemyDispatcherModule =

    type EnemyDispatcher () =
        inherit CharacterDispatcher ()

        override this.Register (entity, world) =
            let world = base.Register (entity, world)
            let characterState = { CharacterState.empty with HitPoints = 10; ControlType = Chaos }
            entity.SetCharacterModel { CharacterModel.initial with CharacterState = characterState; DesiredTurnOpt = Some NoTurn } world