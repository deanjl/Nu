namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module EnemyDispatcherModule =

    type Entity with

        member this.GetDesiredTurn = this.Get Property? DesiredTurn
        member this.SetDesiredTurn = this.Set Property? DesiredTurn
        member this.DesiredTurn = lens<Turn> Property? DesiredTurn this.GetDesiredTurn this.SetDesiredTurn this

    type EnemyDispatcher () =
        inherit CharacterDispatcher ()

        static member Properties =
            [define Entity.CharacterState { CharacterState.empty with HitPoints = 10; ControlType = Chaos }
             define Entity.DesiredTurn NoTurn]

        override this.Register (entity, world) =
            let world = base.Register (entity, world)
            let characterState = { CharacterState.empty with HitPoints = 10; ControlType = Chaos }
            entity.SetCharacterModel { CharacterModel.initial with CharacterState = characterState } world