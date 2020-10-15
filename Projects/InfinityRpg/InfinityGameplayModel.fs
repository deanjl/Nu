namespace InfinityRpg
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

    type MultiRoundMove =
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

    type ChessboardModel =
        { PassableCoordinates : Map<Vector2i, PickupType Option>
          CharacterCoordinates : Map<CharacterIndex, Vector2i>
          CurrentMoves : Map<CharacterIndex, Move> }

        static member empty =
            { PassableCoordinates = Map.empty
              CharacterCoordinates = Map.empty
              CurrentMoves = Map.empty }

        member this.EnemyCoordinates =
            Map.filter (fun (k : CharacterIndex) _ -> k.isEnemy ) this.CharacterCoordinates

        member this.PlayerCoordinates =
            Map.filter (fun (k : CharacterIndex) _ -> not k.isEnemy ) this.CharacterCoordinates
        
        member this.PickupItems =
            Map.filter (fun _ v -> v <> None) this.PassableCoordinates

        member this.EnemyCount =
            this.EnemyCoordinates.Count

        member this.PickupCount =
            this.PickupItems.Count
        
        member this.AvailableCoordinates =
            let occupiedCoordinates = Map.toValueSeq this.CharacterCoordinates
            let passableCoordinates = Map.toKeyList this.PassableCoordinates
            List.except occupiedCoordinates passableCoordinates

        member this.OpenDirections coordinates =
            List.filter (fun d -> List.exists (fun x -> x = (coordinates + (dtovm d))) this.AvailableCoordinates) [Upward; Rightward; Downward; Leftward]
        
        static member updatePassableCoordinates newValue chessboard =
            { chessboard with PassableCoordinates = newValue }
        
        static member updateCharacterCoordinates newValue chessboard =
            { chessboard with CharacterCoordinates = newValue }

        static member updateCurrentMoves newValue chessboard =
            { chessboard with CurrentMoves = newValue }

        static member characterExists index chessboard =
            Map.exists (fun k _ -> k = index) chessboard.CharacterCoordinates
        
        static member pickupAtCoordinates coordinates chessboard =
            match chessboard.PassableCoordinates.[coordinates] with
            | Some _ -> true
            | None -> false
        
        static member updateCoordinatesValue newValue coordinates chessboard =
            let passableCoordinates = Map.add coordinates newValue chessboard.PassableCoordinates
            ChessboardModel.updatePassableCoordinates passableCoordinates chessboard
        
        static member clearPickups _ _ chessboard =
            let passableCoordinates = Map.map (fun _ _ -> None) chessboard.PassableCoordinates
            ChessboardModel.updatePassableCoordinates passableCoordinates chessboard
        
        // used for both adding and relocating
        static member placeCharacter index coordinates (chessboard : ChessboardModel) =
            if List.exists (fun x -> x = coordinates) chessboard.AvailableCoordinates then
                let characterCoordinates = Map.add index coordinates chessboard.CharacterCoordinates
                ChessboardModel.updateCharacterCoordinates characterCoordinates chessboard
            else failwith "character placement failed; coordinates unavailable"

        static member removeCharacter index _ chessboard =
            let characterCoordinates = Map.remove index chessboard.CharacterCoordinates
            ChessboardModel.updateCharacterCoordinates characterCoordinates chessboard
        
        static member clearEnemies _ _ (chessboard : ChessboardModel) =
            ChessboardModel.updateCharacterCoordinates chessboard.PlayerCoordinates chessboard
        
        static member addMove index move chessboard =
            let currentMoves = Map.add index move chessboard.CurrentMoves
            ChessboardModel.updateCurrentMoves currentMoves chessboard
        
        static member setPassableCoordinates _ fieldMap chessboard =
            let passableCoordinates = fieldMap.FieldTiles |> Map.filter (fun _ fieldTile -> fieldTile.TileType = Passable) |> Map.map (fun _ _ -> None)
            ChessboardModel.updatePassableCoordinates passableCoordinates chessboard                    
    
    type [<StructuralEquality; NoComparison>] GameplayModel =
        { MapModeler : MapModeler
          Chessboard : ChessboardModel
          ShallLoadGame : bool
          Field : FieldModel
          PickupModels : PickupModel list
          EnemyModels : CharacterModel list
          PlayerModel : CharacterModel }

        static member initial =
            { MapModeler = MapModeler.make
              Chessboard = ChessboardModel.empty
              ShallLoadGame = false
              Field = FieldModel.initial
              PickupModels = []
              EnemyModels = []
              PlayerModel = CharacterModel.initial }

        member this.PickupModelCount =
            this.PickupModels.Length

        member this.EnemyModelCount =
            this.EnemyModels.Length
        
        static member updateMapModeler newValue model =
            { model with MapModeler = newValue }
        
        static member updateField newValue model =
            { model with Field = newValue }

        static member updatePickupModels newValue model =
            { model with PickupModels = newValue }
        
        static member updateEnemyModels newValue model =
            { model with EnemyModels = newValue }

        static member updatePlayerModel newValue model =
            { model with PlayerModel = newValue }
        
        static member getCharacters model =
            model.PlayerModel :: model.EnemyModels
        
        static member pickupAtCoordinates coordinates model =
            model.PickupModels |> List.exists (fun model -> model.Position = vmtovf coordinates)

        static member characterExists index model =
            GameplayModel.getCharacters model |> List.exists (fun model -> model.Index = index)
        
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
            List.map (fun model -> model.Index) model.EnemyModels

        static member getOpponentIndices index model =
            match index with
            | PlayerIndex -> GameplayModel.getEnemyIndices model
            | _ -> [PlayerIndex]
        
        static member getEnemyTurns model =
            List.map (fun model -> model.Turn) model.EnemyModels
        
        static member getCharacterTurnStati model =
            GameplayModel.getCharacters model |> List.map (fun model -> model.TurnStatus)
        
        static member anyTurnsInProgress model =
            GameplayModel.getCharacterTurnStati model |> List.exists (fun turnStatus -> turnStatus <> Idle)
        
        static member updateCharacterBy updater index newValue model =
            match index with
            | PlayerIndex ->
                let player = updater newValue model.PlayerModel
                GameplayModel.updatePlayerModel player model
            | EnemyIndex _ as index ->
                let enemies =
                    model.EnemyModels |>
                    List.map (fun model -> if model.Index = index then updater newValue model else model)
                GameplayModel.updateEnemyModels enemies model
        
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
            let enemies = List.map2 (fun newValue model -> updater newValue model) newValues model.EnemyModels
            GameplayModel.updateEnemyModels enemies model

        static member updateEnemyActivityStates newValues model =
            GameplayModel.updateEnemiesBy CharacterModel.updateCharacterActivityState newValues model
        
        static member getCoordinates index model =
            model.Chessboard.CharacterCoordinates.[index]

        static member getIndexByCoordinates coordinates model =
            Map.findKey (fun _ x -> x = coordinates) model.Chessboard.CharacterCoordinates

        static member getCurrentMove index model =
            model.Chessboard.CurrentMoves.[index]
        
        static member createPlayerModel model =
            let coordinates = GameplayModel.getCoordinates PlayerIndex model
            let player = CharacterModel.makePlayer coordinates
            GameplayModel.updatePlayerModel player model

        // a basic sync mechanism that relies on never adding and removing *at the same time*
        static member syncModelLists (model : GameplayModel) =
            let chessboard = model.Chessboard
            let model =
                if model.PickupModelCount <> chessboard.PickupCount then
                    let pickups =
                        if model.PickupModelCount > chessboard.PickupCount then
                            List.filter (fun (pickupModel : PickupModel) -> ChessboardModel.pickupAtCoordinates (vftovm pickupModel.Position) chessboard) model.PickupModels
                        else 
                            let generator k _ = PickupModel.makeHealth k
                            let pickups = Map.filter (fun k _ -> not (GameplayModel.pickupAtCoordinates k model)) chessboard.PickupItems |> Map.toListBy generator
                            pickups @ model.PickupModels
                    GameplayModel.updatePickupModels pickups model
                else model

            if model.EnemyModelCount <> chessboard.EnemyCount then
                let enemies =
                    if model.EnemyModelCount > chessboard.EnemyCount then
                        List.filter (fun (characterModel : CharacterModel) -> ChessboardModel.characterExists characterModel.Index chessboard) model.EnemyModels
                    else
                        let generator k v = CharacterModel.makeEnemy k v
                        let enemies = Map.filter (fun k _ -> not (GameplayModel.characterExists k model)) chessboard.EnemyCoordinates |> Map.toListBy generator
                        enemies @ model.EnemyModels
                GameplayModel.updateEnemyModels enemies model
            else model

        // if updater takes index, index is arg1; if updater takes coordinates, coordinates is arg2
        static member updateChessboardBy updater arg1 arg2 model =
            let chessboard = updater arg1 arg2 model.Chessboard
            let model = { model with Chessboard = chessboard }
            GameplayModel.syncModelLists model
        
        static member relocateCharacter index coordinates model =
            GameplayModel.updateChessboardBy ChessboardModel.placeCharacter index coordinates model
        
        static member addMove index (move : Move) model =
            GameplayModel.updateChessboardBy ChessboardModel.addMove index move model
        
        static member addHealth coordinates model =
            GameplayModel.updateChessboardBy ChessboardModel.updateCoordinatesValue (Some Health) coordinates model

        static member removeHealth coordinates model =
            GameplayModel.updateChessboardBy ChessboardModel.updateCoordinatesValue None coordinates model
        
        static member clearPickups model =
            GameplayModel.updateChessboardBy ChessboardModel.clearPickups () () model
        
        static member removeEnemy index model =
            let coordinates = GameplayModel.getCoordinates index model
            let model = GameplayModel.addHealth coordinates model
            GameplayModel.updateChessboardBy ChessboardModel.removeCharacter index () model

        static member clearEnemies model =
            GameplayModel.updateChessboardBy ChessboardModel.clearEnemies () () model

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
                let model = GameplayModel.updateCharacterState index { model.PlayerModel.CharacterState with HitPoints = 30 } model
                GameplayModel.removeHealth coordinates model
            | _ -> model
        
        static member applyStep index direction model =
            let coordinates = (GameplayModel.getCoordinates index model) + dtovm direction
            let model =
                if ChessboardModel.pickupAtCoordinates coordinates model.Chessboard then
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
            let model = GameplayModel.updateChessboardBy ChessboardModel.setPassableCoordinates () fieldMap model
            let fieldModel = { FieldMapNp = fieldMap }
            GameplayModel.updateField fieldModel model

        static member transitionMap direction model =
            let mapModeler = model.MapModeler.Transition direction
            GameplayModel.updateMapModeler mapModeler model

        static member setCharacterPositionToCoordinates index model =
            let position = GameplayModel.getCoordinates index model |> vmtovf
            GameplayModel.updatePosition index position model
        
        static member yankPlayer coordinates model =
            let model = GameplayModel.relocateCharacter PlayerIndex coordinates model
            GameplayModel.setCharacterPositionToCoordinates PlayerIndex model
        
        static member makePlayer model =
            let model = GameplayModel.updateChessboardBy ChessboardModel.placeCharacter PlayerIndex Vector2i.Zero model
            GameplayModel.createPlayerModel model

        static member makeEnemy index model =
            let availableCoordinates = model.Chessboard.AvailableCoordinates
            let coordinates = availableCoordinates.Item(Gen.random1 availableCoordinates.Length)
            GameplayModel.updateChessboardBy ChessboardModel.placeCharacter index coordinates model

        static member makeEnemies quantity model =
            let quantity = quantity - 1
            let rec recursion count model =
                let model = GameplayModel.makeEnemy (EnemyIndex count) model
                if count = quantity then model
                else recursion (count + 1) model
            recursion 0 model
        
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