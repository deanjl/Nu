﻿namespace InfinityRpg
open System
open System.IO
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module GameplayModelModule =

    type FieldMapUnit =
        { RandSeed : uint64
          OffsetCount : Vector2i
          IsHorizontal : bool
          PathStart : Vector2i
          PathEnd : Vector2i }

        member this.ToFieldMap =
            let rand = Rand.makeFromSeedState this.RandSeed
            FieldMap.make Assets.FieldTileSheetImage Vector2i.Zero Constants.Layout.FieldUnitSizeM [(this.PathStart, this.PathEnd)] rand |> fst

        static member make fieldMapUnitOpt =
            let sysrandom = System.Random ()
            let randSeed = uint64 (sysrandom.Next ())
            let randResult = Gen.random1 (Constants.Layout.FieldUnitSizeM.X - 4) // assumes X and Y are equal
            let pathEnd = if randResult % 2 = 0 then Vector2i (randResult + 2, Constants.Layout.FieldUnitSizeM.Y - 2) else Vector2i (Constants.Layout.FieldUnitSizeM.X - 2, randResult + 2)
            let (offsetCount, pathStart) =
                match fieldMapUnitOpt with
                | Some fieldMapUnit ->
                    match fieldMapUnit.IsHorizontal with
                    | true -> (fieldMapUnit.OffsetCount + Vector2i.Right, Vector2i (1, fieldMapUnit.PathEnd.Y))
                    | false -> (fieldMapUnit.OffsetCount + Vector2i.Up, Vector2i (fieldMapUnit.PathEnd.X, 1))
                | None -> (Vector2i.Zero, Vector2i.One)
            
            { RandSeed = randSeed
              OffsetCount = offsetCount
              IsHorizontal = pathEnd.X > pathEnd.Y
              PathStart = pathStart
              PathEnd = pathEnd }
    
    type MapModeler =
        { FieldMapUnits : Map<Vector2i, FieldMapUnit>
          CurrentFieldOffset : Vector2i }

        static member empty =
            { FieldMapUnits = Map.empty
              CurrentFieldOffset = Vector2i.Zero }

        member this.AddFieldMapUnit fieldMapUnit =
            let fieldMapUnits = Map.add fieldMapUnit.OffsetCount fieldMapUnit this.FieldMapUnits
            { this with FieldMapUnits = fieldMapUnits; CurrentFieldOffset = fieldMapUnit.OffsetCount }

        member this.GetCurrent =
            this.FieldMapUnits.[this.CurrentFieldOffset]

        member this.OffsetInDirection direction =
            this.CurrentFieldOffset + dtovm direction
        
        member this.ExistsInDirection direction =
            Map.containsKey (this.OffsetInDirection direction) this.FieldMapUnits
        
        member this.NextOffset =
            if this.GetCurrent.IsHorizontal then
                this.OffsetInDirection Rightward
            else this.OffsetInDirection Upward
        
        member this.NextOffsetInDirection direction =
            this.NextOffset = this.OffsetInDirection direction

        member this.PossibleInDirection direction =
            this.ExistsInDirection direction || this.NextOffsetInDirection direction
        
        member this.MoveCurrent direction =
            { this with CurrentFieldOffset = this.OffsetInDirection direction }
        
        member this.MakeFieldMapUnit =
            FieldMapUnit.make (Some this.GetCurrent) |> this.AddFieldMapUnit
        
        member this.Transition direction =
            if this.ExistsInDirection direction then
                this.MoveCurrent direction
            else this.MakeFieldMapUnit
        
        static member make =
            FieldMapUnit.make None |> MapModeler.empty.AddFieldMapUnit

    type SingleRoundMove =
        | Step of Direction
        | Attack of CharacterIndex

    type MultiRoundMove = // must be reducible to SingleRoundMoves e.g. Travel = { Step, Step ...}
        | Travel of NavigationNode list

    type Move =
        | SingleRoundMove of SingleRoundMove
        | MultiRoundMove of MultiRoundMove

        member this.MakeTurn positionM =
            match this with
            | SingleRoundMove singleRoundMove ->
                match singleRoundMove with
                | Step direction -> Turn.makeNavigation None positionM direction
                | Attack index -> Turn.makeAttack index
            | MultiRoundMove multiRoundMove ->
                match multiRoundMove with
                | Travel path ->
                    let direction = directionToTarget positionM path.Head.PositionM
                    Turn.makeNavigation (Some path) positionM direction

    type MoveModeler =
        { PassableCoordinates : Map<Vector2i, PickupType Option>
          CharacterCoordinates : Map<CharacterIndex, Vector2i>
          CurrentMoves : Map<CharacterIndex, Move> }

        static member empty =
            { PassableCoordinates = Map.empty
              CharacterCoordinates = Map.empty
              CurrentMoves = Map.empty }

        member this.AvailableCoordinates =
            let occupiedCoordinates = Map.toValueSeq this.CharacterCoordinates
            let passableCoordinates = Map.toKeyList this.PassableCoordinates
            List.except occupiedCoordinates passableCoordinates

        member this.OpenDirections coordinates =
            List.filter (fun d -> List.exists (fun x -> x = (coordinates + (dtovm d))) this.AvailableCoordinates) [Upward; Rightward; Downward; Leftward]
        
        static member updatePassableCoordinates newValue moveModeler =
            { moveModeler with PassableCoordinates = newValue }
        
        static member updateCharacterCoordinates newValue moveModeler =
            { moveModeler with CharacterCoordinates = newValue }

        static member updateCurrentMoves newValue moveModeler =
            { moveModeler with CurrentMoves = newValue }

        static member updateCoordinatesValue coordinates newValue moveModeler =
            let passableCoordinates = Map.add coordinates newValue moveModeler.PassableCoordinates
            MoveModeler.updatePassableCoordinates passableCoordinates moveModeler
        
        static member addHealth coordinates moveModeler =
            MoveModeler.updateCoordinatesValue coordinates (Some Health) moveModeler

        static member removeHealth coordinates moveModeler =
            MoveModeler.updateCoordinatesValue coordinates None moveModeler

        static member pickupAtCoordinates coordinates moveModeler =
            match moveModeler.PassableCoordinates.[coordinates] with
            | Some _ -> true
            | None -> false
        
        static member clearPickups moveModeler =
            let passableCoordinates = Map.map (fun _ _ -> None) moveModeler.PassableCoordinates
            MoveModeler.updatePassableCoordinates passableCoordinates moveModeler
        
        // used for both adding and relocating
        static member placeCharacter index coordinates (moveModeler : MoveModeler) =
            if List.exists (fun x -> x = coordinates) moveModeler.AvailableCoordinates then
                let characterCoordinates = Map.add index coordinates moveModeler.CharacterCoordinates
                MoveModeler.updateCharacterCoordinates characterCoordinates moveModeler
            else failwith "character placement failed; coordinates unavailable"

        static member removeCharacter index moveModeler =
            let characterCoordinates = Map.remove index moveModeler.CharacterCoordinates
            MoveModeler.updateCharacterCoordinates characterCoordinates moveModeler
        
        static member clearEnemies moveModeler =
            let characterCoordinates = Map.filter (fun (k : CharacterIndex) _ -> not k.isEnemy ) moveModeler.CharacterCoordinates
            MoveModeler.updateCharacterCoordinates characterCoordinates moveModeler
        
        static member addMove index move moveModeler =
            let currentMoves = Map.add index move moveModeler.CurrentMoves
            MoveModeler.updateCurrentMoves currentMoves moveModeler
        
        static member setPassableCoordinates fieldMap moveModeler =
            let passableCoordinates = fieldMap.FieldTiles |> Map.filter (fun _ fieldTile -> fieldTile.TileType = Passable) |> Map.map (fun _ _ -> None)
            MoveModeler.updatePassableCoordinates passableCoordinates moveModeler                    
    
    type [<StructuralEquality; NoComparison>] GameplayModel =
        { MapModeler : MapModeler
          MoveModeler : MoveModeler
          ShallLoadGame : bool
          Field : FieldModel
          PickupItems : PickupModel list
          Enemies : CharacterModel list
          Player : CharacterModel }

        static member initial =
            { MapModeler = MapModeler.make
              MoveModeler = MoveModeler.empty
              ShallLoadGame = false
              Field = FieldModel.initial
              PickupItems = []
              Enemies = []
              Player = CharacterModel.initial }

        static member updatePickupItems newValue model =
            { model with PickupItems = newValue }
        
        static member getCharacters model =
            model.Player :: model.Enemies
        
        static member tryGetCharacterByIndex index model =
            GameplayModel.getCharacters model |> List.tryFind (fun model -> model.Index = index)
        
        static member getCharacterByIndex index model =
            GameplayModel.tryGetCharacterByIndex index model |> Option.get

        static member getTurn index model =
            (GameplayModel.getCharacterByIndex index model).Turn

        static member getCharacterState index model =
            (GameplayModel.getCharacterByIndex index model).CharacterState
        
        static member getTurnStatus index model =
            (GameplayModel.getCharacterByIndex index model).TurnStatus
        
        static member getCharacterActivityState index model =
            (GameplayModel.getCharacterByIndex index model).CharacterActivityState

        static member getCharacterAnimationState index model =
            (GameplayModel.getCharacterByIndex index model).CharacterAnimationState
        
        static member getPosition index model =
            (GameplayModel.getCharacterByIndex index model).Position
        
        static member getEnemyIndices model =
            List.map (fun model -> model.Index) model.Enemies

        static member getOpponentIndices index model =
            match index with
            | PlayerIndex -> GameplayModel.getEnemyIndices model
            | _ -> [PlayerIndex]
        
        static member getEnemyTurns model =
            List.map (fun model -> model.Turn) model.Enemies
        
        static member getCharacterTurnStati model =
            GameplayModel.getCharacters model |> List.map (fun model -> model.TurnStatus)
        
        static member anyTurnsInProgress model =
            GameplayModel.getCharacterTurnStati model |> List.exists (fun turnStatus -> turnStatus <> Idle)
        
        static member updateCharacterBy updater index newValue model =
            match index with
            | PlayerIndex -> { model with Player = updater newValue model.Player }
            | EnemyIndex _ as index ->
                let enemies =
                    model.Enemies |>
                    List.map (fun model -> if model.Index = index then updater newValue model else model)
                { model with Enemies = enemies }
        
        static member updateTurn index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateTurn index newValue model

        static member updateCharacterState index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateCharacterState index newValue model
        
        static member updateTurnStatus index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateTurnStatus index newValue model
        
        static member updateCharacterActivityState index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateCharacterActivityState index newValue model

        static member updateCharacterAnimationState index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateCharacterAnimationState index newValue model

        static member updatePosition index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updatePosition index newValue model
        
        static member updateEnemiesBy updater newValues model =
            let enemies = List.map2 (fun newValue model -> updater newValue model) newValues model.Enemies
            { model with Enemies = enemies }

        static member updateEnemyActivityStates newValues model =
            GameplayModel.updateEnemiesBy CharacterModel.updateCharacterActivityState newValues model
        
        static member getCoordinates index model =
            model.MoveModeler.CharacterCoordinates.[index]

        static member getIndexByCoordinates coordinates model =
            Map.findKey (fun _ x -> x = coordinates) model.MoveModeler.CharacterCoordinates

        static member getCurrentMove index model =
            model.MoveModeler.CurrentMoves.[index]
        
        static member relocateCharacter index coordinates model =
            { model with MoveModeler = MoveModeler.placeCharacter index coordinates model.MoveModeler }
        
        static member addMove index (move : Move) model =
            { model with MoveModeler = MoveModeler.addMove index move model.MoveModeler }
        
        static member addHealth coordinates model =
            let pickupItems = PickupModel.makeHealth coordinates :: model.PickupItems
            let model = GameplayModel.updatePickupItems pickupItems model
            let moveModeler = MoveModeler.addHealth coordinates model.MoveModeler
            { model with MoveModeler = moveModeler }

        static member removeHealth coordinates model =
            let pickupItems = List.filter (fun (model : PickupModel) -> model.Position <> vmtovf coordinates) model.PickupItems
            let model = GameplayModel.updatePickupItems pickupItems model
            let moveModeler = MoveModeler.removeHealth coordinates model.MoveModeler
            { model with MoveModeler = moveModeler }
        
        static member clearPickups model =
            let model = GameplayModel.updatePickupItems [] model
            let moveModeler = MoveModeler.clearPickups model.MoveModeler
            { model with MoveModeler = moveModeler }
        
        static member removeEnemy index model =
            let coordinates = GameplayModel.getCoordinates index model
            let model = GameplayModel.addHealth coordinates model
            let enemies = List.filter (fun model -> model.Index <> index) model.Enemies
            let moveModeler = MoveModeler.removeCharacter index model.MoveModeler
            { model with MoveModeler = moveModeler; Enemies = enemies }

        static member clearEnemies model =
            { model with MoveModeler = MoveModeler.clearEnemies model.MoveModeler; Enemies = [] }

        static member unpackMove index model =
            let move = GameplayModel.getCurrentMove index model
            let turn = GameplayModel.getCoordinates index model |> move.MakeTurn
            let model = GameplayModel.updateTurn index turn model
            GameplayModel.updateTurnStatus index TurnPending model
        
        static member finishMove index model =
            let model = GameplayModel.updateTurn index NoTurn model
            let model = GameplayModel.updateCharacterActivityState index NoActivity model
            GameplayModel.updateTurnStatus index Idle model
        
        static member tryPickupHealth index coordinates model =
            match index with
            | PlayerIndex ->
                let model = GameplayModel.updateCharacterState index { model.Player.CharacterState with HitPoints = 30 } model
                GameplayModel.removeHealth coordinates model
            | _ -> model
        
        static member applyStep index direction model =
            let coordinates = (GameplayModel.getCoordinates index model) + dtovm direction
            let model =
                if MoveModeler.pickupAtCoordinates coordinates model.MoveModeler then
                    GameplayModel.tryPickupHealth index coordinates model
                else model
            GameplayModel.relocateCharacter index coordinates model
        
        static member applyAttack reactorIndex model =
            let reactorDamage = 4 // NOTE: just hard-coding damage for now
            let reactorState = GameplayModel.getCharacterState reactorIndex model
            GameplayModel.updateCharacterState reactorIndex { reactorState with HitPoints = reactorState.HitPoints - reactorDamage } model
        
        static member stopTraveler reactorIndex model =
            match GameplayModel.getTurn reactorIndex model with
            | NavigationTurn navigationDescriptor ->
                GameplayModel.updateTurn reactorIndex (NavigationTurn navigationDescriptor.CancelNavigation) model
            | _ -> model
        
        static member applyMove index model =
            let move = GameplayModel.getCurrentMove index model
            match move with
            | SingleRoundMove singleRoundMove ->
                match singleRoundMove with
                | Step direction -> GameplayModel.applyStep index direction model
                | Attack reactorIndex ->
                    let model = GameplayModel.applyAttack reactorIndex model
                    GameplayModel.stopTraveler reactorIndex model
            | MultiRoundMove multiRoundMove ->
                match multiRoundMove with
                | Travel (head :: _) ->
                    let currentCoordinates = GameplayModel.getCoordinates index model
                    let direction = directionToTarget currentCoordinates head.PositionM
                    GameplayModel.applyStep index direction model
        
        static member activateCharacter index model =
            let activity = GameplayModel.getTurn index model |> Turn.toCharacterActivityState
            let model = GameplayModel.updateCharacterActivityState index activity model
            GameplayModel.updateTurnStatus PlayerIndex TurnBeginning model
        
        static member setFieldMap fieldMap model =
            let moveModeler = MoveModeler.setPassableCoordinates fieldMap model.MoveModeler
            let fieldModel = { FieldMapNp = fieldMap }
            { model with MoveModeler = moveModeler; Field = fieldModel }

        static member transitionMap direction model =
            { model with MapModeler = model.MapModeler.Transition direction }

        static member yankPlayer coordinates model =
            let model = GameplayModel.relocateCharacter PlayerIndex coordinates model
            GameplayModel.updatePosition PlayerIndex (vmtovf coordinates) model
        
        static member makePlayer model =
            let coordinates = Vector2i.Zero
            let moveModeler = MoveModeler.placeCharacter PlayerIndex coordinates model.MoveModeler
            let playerModel = CharacterModel.makePlayer coordinates
            { model with MoveModeler = moveModeler; Player = playerModel }

        static member makeEnemy index model =
            let availableCoordinates = model.MoveModeler.AvailableCoordinates
            let coordinates = availableCoordinates.Item(Gen.random1 availableCoordinates.Length)
            let moveModeler = MoveModeler.placeCharacter index coordinates model.MoveModeler
            let enemyModel = CharacterModel.makeEnemy index coordinates
            { model with MoveModeler = moveModeler; Enemies = enemyModel :: model.Enemies }

        static member makeEnemies quantity model =
            let quantity = quantity - 1
            let rec recursion count model =
                let model = GameplayModel.makeEnemy (EnemyIndex count) model
                if count = quantity then model
                else recursion (count + 1) model
            let model = recursion 0 model
            { model with Enemies = List.rev model.Enemies } // enemy indices in ascending order because enemy turns must be applied in the same order as they are determined
        
        static member forEachIndex updater indices model =
            let rec recursion (indices : CharacterIndex list) model =
                if indices.Length = 0 then model
                else
                    let index = indices.Head
                    let characterModelOpt = GameplayModel.tryGetCharacterByIndex index model
                    let model =
                        match characterModelOpt with
                        | None -> model
                        | Some _ -> updater index model
                    recursion indices.Tail model
            recursion indices model