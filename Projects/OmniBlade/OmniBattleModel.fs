﻿namespace OmniBlade
open FSharpx.Collections
open Prime
open Nu

type BattleState =
    | BattleReady of int64
    | BattleRunning
    | BattleCease of bool * int64

type ActionCommand =
    { Action : ActionType
      Source : CharacterIndex
      TargetOpt : CharacterIndex option }

    static member make action source targetOpt =
        { Action = action
          Source = source
          TargetOpt = targetOpt }

type CurrentCommand =
    { TimeStart : int64
      ActionCommand : ActionCommand }

    static member make timeStart actionCommand =
        { TimeStart = timeStart; ActionCommand = actionCommand }

[<RequireQualifiedAccess>]
module BattleModel =

    type [<ReferenceEquality; NoComparison>] BattleModel =
        private
            { BattleState_ : BattleState
              Characters : Map<CharacterIndex, CharacterModel>
              Inventory_ : Inventory
              Gold_ : int
              CurrentCommandOpt_ : CurrentCommand option
              ActionCommands_ : ActionCommand Queue }

        member this.BattleState = this.BattleState_
        member this.Inventory = this.Inventory_
        member this.Gold = this.Gold_
        member this.CurrentCommandOpt = this.CurrentCommandOpt_
        member this.ActionCommands = this.ActionCommands_

        static member getAllies model =
            model.Characters |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

        static member getEnemies model =
            model.Characters |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

        static member getAlliesHealthy model =
            BattleModel.getAllies model |>
            List.filter (fun character -> character.IsHealthy)

        static member getAlliesWounded model =
            BattleModel.getAllies model |>
            List.filter (fun character -> character.IsWounded)

        static member getAllyIndices model =
            BattleModel.getAllies model |>
            List.map (fun ally -> ally.CharacterIndex)

        static member getEnemyIndices model =
            BattleModel.getEnemies model |>
            List.map (fun enemy -> enemy.CharacterIndex)

        static member getAlliesHealthyIndices model =
            BattleModel.getAlliesHealthy model |>
            List.map (fun ally -> ally.CharacterIndex)

        static member getAlliesWoundedIndices model =
            BattleModel.getAlliesWounded model |>
            List.map (fun enemy -> enemy.CharacterIndex)

        static member getAllyIndexRandom model =
            let alliesHealthyIndices = BattleModel.getAlliesHealthyIndices model
            List.item (Gen.random1 alliesHealthyIndices.Length) alliesHealthyIndices

        static member getEnemyIndexRandom model =
            let enemyIndices = BattleModel.getEnemyIndices model
            let enemyIndex = List.item (Gen.random1 enemyIndices.Length) enemyIndices
            enemyIndex

        static member getTargets aimType model =
            match aimType with
            | EnemyAim _ ->
                BattleModel.getEnemies model
            | AllyAim healthy ->
                if healthy
                then BattleModel.getAlliesHealthy model
                else BattleModel.getAlliesWounded model
            | AnyAim healthy ->
                let allies =
                    if healthy
                    then BattleModel.getAlliesHealthy model
                    else BattleModel.getAlliesWounded model
                let enemies = BattleModel.getEnemies model
                let characters = allies @ enemies
                characters
            | NoAim -> []

        static member addCharacter index character (model : BattleModel) =
            { model with Characters = Map.add index character model.Characters }

        static member removeCharacter index (model : BattleModel) =
            { model with Characters = Map.remove index model.Characters }

        static member updateCharactersIf predicate updater model =
            { model with BattleModel.Characters = Map.map (fun index character -> if predicate index then updater character else character) model.Characters }

        static member updateCharacters updater model =
            BattleModel.updateCharactersIf tautology updater model

        static member updateAllies updater model =
            BattleModel.updateCharactersIf (function AllyIndex _ -> true | _ -> false) updater model

        static member updateEnemies updater model =
            BattleModel.updateCharactersIf (function EnemyIndex _ -> true | _ -> false) updater model

        static member getCharacters model =
            model.Characters |> Map.toValueList

        static member tryGetCharacter characterIndex model =
            Map.tryFind characterIndex model.Characters

        static member getCharacter characterIndex model =
            BattleModel.tryGetCharacter characterIndex model |> Option.get

        static member tryUpdateCharacter updater characterIndex model =
            match BattleModel.tryGetCharacter characterIndex model with
            | Some character ->
                let character = updater character
                { model with Characters = Map.add characterIndex character model.Characters }
            | None -> model

        static member updateCharacter updater characterIndex model =
            let character = BattleModel.getCharacter characterIndex model
            let character = updater character
            { model with Characters = Map.add characterIndex character model.Characters }

        static member updateBattleState updater model =
            { model with BattleState_ = updater model.BattleState_ }

        static member updateInventory updater model =
            { model with Inventory_ = updater model.Inventory_ }

        static member updateGold updater model =
            { model with Gold_ = updater model.Gold_ }

        static member updateCurrentCommandOpt updater model =
            { model with CurrentCommandOpt_ = updater model.CurrentCommandOpt_ }

        static member updateActionCommands updater model =
            { model with ActionCommands_ = updater model.ActionCommands_ }

        static member appendActionCommand command model =
            { model with ActionCommands_ = Queue.conj command model.ActionCommands }

        static member prependActionCommand command model =
            { model with ActionCommands_ = Queue.rev model.ActionCommands |> Queue.conj command |> Queue.rev }

        static member make allies inventory gold battleType time =
            match Map.tryFind battleType data.Value.Battles with
            | Some battleData ->
                let enemies = List.mapi CharacterModel.makeEnemy battleData.BattleEnemies
                let characters = allies @ enemies |> Map.ofListBy (fun (character : CharacterModel) -> (character.CharacterIndex, character))
                let model =
                    { BattleState_ = BattleReady time
                      Characters = characters
                      Inventory_ = inventory
                      Gold_ = gold
                      CurrentCommandOpt_ = None
                      ActionCommands_ = Queue.empty }
                model
            | None -> BattleModel.empty

        static member empty =
            { BattleState_ = BattleReady 0L
              Characters = Map.empty
              Inventory_ = { Items = Map.empty }
              Gold_ = 0
              CurrentCommandOpt_ = None
              ActionCommands_ = Queue.empty }

type BattleModel = BattleModel.BattleModel