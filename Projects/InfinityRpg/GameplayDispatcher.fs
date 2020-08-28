namespace InfinityRpg
open System
open System.IO
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module GameplayDispatcherModule =

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
                    let direction = vmtod (path.Head.PositionM - positionM)
                    Turn.makeNavigation (Some path) positionM direction

    type MoveModeler =
        { PassableCoordinates : Vector2i list
          CharacterCoordinates : Map<CharacterIndex, Vector2i>
          LatestSingleMoves : Map<CharacterIndex, SingleRoundMove>
          LatestMultiMoves : Map<CharacterIndex, MultiRoundMove> }

        static member empty =
            { PassableCoordinates = []
              CharacterCoordinates = Map.empty
              LatestSingleMoves = Map.empty
              LatestMultiMoves = Map.empty }

        member this.AvailableCoordinates =
            let occupiedCoordinates = Map.toValueSeq this.CharacterCoordinates
            List.except occupiedCoordinates this.PassableCoordinates

        member this.OpenDirections coordinates =
            List.filter (fun d -> List.exists (fun x -> x = (coordinates + (dtovm d))) this.AvailableCoordinates) [Upward; Rightward; Downward; Leftward]
        
        member this.AddCharacter index coordinates =
            if List.exists (fun x -> x = coordinates) this.AvailableCoordinates then
                { this with CharacterCoordinates = Map.add index coordinates this.CharacterCoordinates }
            else failwith "character placement failed; coordinates unavailable"

        member this.RemoveCharacter index =
            { this with CharacterCoordinates = Map.remove index this.CharacterCoordinates }
        
        member this.RelocateCharacter index coordinates =
            if List.exists (fun x -> x = coordinates) this.AvailableCoordinates then
                let characterCoordinates = Map.add index coordinates this.CharacterCoordinates
                { this with CharacterCoordinates = characterCoordinates }
            else failwith "character relocation failed; coordinates unavailable"
        
        member this.AddSingleMove index move =
            { this with LatestSingleMoves = Map.add index move this.LatestSingleMoves }

        member this.AddMultiMove index move =
            { this with LatestMultiMoves = Map.add index move this.LatestMultiMoves }

        member this.AddMove index move =
            match move with
            | SingleRoundMove move -> this.AddSingleMove index move
            | MultiRoundMove move -> this.AddMultiMove index move
        
        static member make fieldMap =
            let passableCoordinates = fieldMap.FieldTiles |> Map.filter (fun _ fieldTile -> fieldTile.TileType = Passable) |> Map.toKeyList
            { MoveModeler.empty with PassableCoordinates = passableCoordinates }                    
    
    type [<StructuralEquality; NoComparison>] GameplayModel =
        { MoveModeler : MoveModeler
          ContentRandState : uint64
          ShallLoadGame : bool
          FieldMapOpt : FieldMap option
          Enemies : CharacterModel list
          Player : CharacterModel }

        static member initial =
            let sysrandom = System.Random ()
            let contentSeedState = uint64 (sysrandom.Next ())
            { MoveModeler = MoveModeler.empty
              ContentRandState = contentSeedState
              ShallLoadGame = false
              FieldMapOpt = None
              Enemies = []
              Player = CharacterModel.initial }

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
        
        static member removeEnemy index model =
            let enemies = List.filter (fun model -> model.Index <> index) model.Enemies
            let moveModeler = model.MoveModeler.RemoveCharacter index
            { model with MoveModeler = moveModeler; Enemies = enemies }

        static member updateEnemiesBy updater newValues model =
            let enemies = List.map2 (fun newValue model -> updater newValue model) newValues model.Enemies
            { model with Enemies = enemies }

        static member updateEnemyActivityStates newValues model =
            GameplayModel.updateEnemiesBy CharacterModel.updateCharacterActivityState newValues model

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

        static member getCoordinates index model =
            model.MoveModeler.CharacterCoordinates.[index]

        static member getIndexByCoordinates coordinates model =
            Map.findKey (fun _ x -> x = coordinates) model.MoveModeler.CharacterCoordinates

        static member getMultiMove index model =
            model.MoveModeler.LatestMultiMoves.[index]
        
        static member relocateCharacter index coordinates model =
            { model with MoveModeler = model.MoveModeler.RelocateCharacter index coordinates }

        static member addMove index (move : Move) model =
            let turn = GameplayModel.getCoordinates index model |> move.MakeTurn
            let model = GameplayModel.updateTurn index turn model
            let model = GameplayModel.updateTurnStatus index TurnPending model
            { model with MoveModeler = model.MoveModeler.AddMove index move }
        
        static member finishMove index model =
            let model = GameplayModel.updateTurn index NoTurn model
            let model = GameplayModel.updateCharacterActivityState index NoActivity model
            GameplayModel.updateTurnStatus index Idle model
        
        static member applyStep index direction model =
            let coordinates = (GameplayModel.getCoordinates index model) + dtovm direction
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
        
        static member activateCharacter index model =
            let activity = GameplayModel.getTurn index model |> Turn.toCharacterActivityState
            let model = GameplayModel.updateCharacterActivityState index activity model
            GameplayModel.updateTurnStatus PlayerIndex TurnBeginning model
        
        static member addFieldMap fieldMap model =
            let moveModeler = MoveModeler.make fieldMap
            { model with MoveModeler = moveModeler; FieldMapOpt = Some fieldMap }

        static member makePlayer model =
            let coordinates = Vector2i.Zero
            let moveModeler = model.MoveModeler.AddCharacter PlayerIndex coordinates
            let playerModel = CharacterModel.makePlayer coordinates
            { model with MoveModeler = moveModeler; Player = playerModel }

        static member makeEnemy index model =
            let availableCoordinates = model.MoveModeler.AvailableCoordinates
            let coordinates = availableCoordinates.Item(Gen.random1 availableCoordinates.Length)
            let moveModeler = model.MoveModeler.AddCharacter index coordinates
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

    type [<StructuralEquality; NoComparison>] PlayerInput =
        | TouchInput of Vector2
        | DetailInput of Direction
        | NoInput

    type [<StructuralEquality; NoComparison>] GameplayMessage =
        | TryContinueNavigation
        | FinishTurns of CharacterIndex list
        | ProgressTurns of CharacterIndex list
        | BeginTurns of CharacterIndex list
        | RunCharacterActivation
        | PlayNewRound of Move
        | TryMakePlayerMove of PlayerInput
        | StartGameplay
    
    type [<NoEquality; NoComparison>] GameplayCommand =
        | ToggleHaltButton // TODO: reimplement once game is properly elmified
        | HandlePlayerInput of PlayerInput
        | SaveGame
        | Tick
        | PostTick
        | Nop

    type Screen with

    (* random number generation is non-deterministic for gameplay behavior and deterministic for map generation. for the field specifically, ContentRandState is necessary for the game saving architecture and functional purity is non-negotiable when relying on lazy evaluation. VERY hard learnt lesson. *)
        
        member this.GetGameplayModel = this.GetModel<GameplayModel>
        member this.SetGameplayModel = this.SetModel<GameplayModel>
        member this.GameplayModel = this.Model<GameplayModel> ()

    type GameplayDispatcher () =
        inherit ScreenDispatcher<GameplayModel, GameplayMessage, GameplayCommand> (GameplayModel.initial)

        static let determinePathEnd rand =
            let (randResult, rand) = Rand.nextIntUnder (Constants.Layout.FieldUnitSizeM.X - 4) rand // assumes X and Y are equal
            let pathEnd = if randResult % 2 = 0 then Vector2i (randResult + 2, Constants.Layout.FieldUnitSizeM.Y - 2) else Vector2i (Constants.Layout.FieldUnitSizeM.X - 2, randResult + 2)
            (pathEnd, rand)

        static let makeFieldUnit (fieldUnitOpt, rand) =
            let (pathEnd, rand) = determinePathEnd rand
            let (offsetCount, pathStart) =
                match fieldUnitOpt with
                | Some fieldUnit ->
                    match fieldUnit.IsHorizontal with
                    | true -> (fieldUnit.OffsetCount + Vector2i.Right, Vector2i (1, fieldUnit.PathEnd.Y))
                    | false -> (fieldUnit.OffsetCount + Vector2i.Up, Vector2i (fieldUnit.PathEnd.X, 1))
                | None -> (Vector2i.Zero, Vector2i.One)
            let fieldUnit =
                { OffsetCount = offsetCount
                  IsHorizontal = pathEnd.X > pathEnd.Y
                  PathStart = pathStart
                  PathEnd = pathEnd }
            (fieldUnit, rand)

        static let fieldUnitToFieldTiles (fieldUnit, rand) =
            let offsetM = Vector2i.Multiply (fieldUnit.OffsetCount, Constants.Layout.FieldUnitSizeM)
            let pathEdgesM = [(offsetM + fieldUnit.PathStart, offsetM + fieldUnit.PathEnd)]
            let (fieldMap, rand) = FieldMap.make Assets.FieldTileSheetImage offsetM Constants.Layout.FieldUnitSizeM pathEdgesM rand
            (fieldMap.FieldTiles, rand)

        static let createField rand =
            let extentions = 0
            let fieldUnitTuples =
                List.fold
                    (fun (fieldUnitTuples : (FieldUnit * Rand) list) _ ->
                        let lastFieldUnitTuple = fieldUnitTuples.Head
                        let lastFieldUnitTupleWithOpt = (Some (fst lastFieldUnitTuple), snd lastFieldUnitTuple)
                        let fieldUnitTuple = makeFieldUnit lastFieldUnitTupleWithOpt
                        fieldUnitTuple :: fieldUnitTuples)
                    [makeFieldUnit (None, rand)]
                    [0 .. extentions - 1]
            let offsetCount = (fst fieldUnitTuples.Head).OffsetCount
            let fieldTilesTupleList = List.map (fun fieldUnitTuple -> fieldUnitToFieldTiles fieldUnitTuple) fieldUnitTuples
            let fieldTiles = List.map (fun fieldTiles -> fst fieldTiles) fieldTilesTupleList |> List.reduce (fun concat fieldTiles -> concat @@ fieldTiles)
            let fieldMap = 
                { FieldSizeM = Constants.Layout.FieldUnitSizeM + Vector2i.Multiply (offsetCount, Constants.Layout.FieldUnitSizeM)
                  FieldTiles = fieldTiles
                  FieldTileSheet = Assets.FieldTileSheetImage }
            fieldMap

        static let tryGetNavigationPath index positionM model =
            let fieldTiles = (Option.get model.FieldMapOpt).FieldTiles
            let characterPositions = Map.toValueList model.MoveModeler.CharacterCoordinates |> List.map (fun positionM -> vmtovf positionM)
            let currentPositionM = GameplayModel.getCoordinates PlayerIndex model
            let occupationMap = OccupationMap.makeFromFieldTilesAndAdjacentCharacters currentPositionM fieldTiles characterPositions
            let nodes = OccupationMap.makeNavigationNodes occupationMap
            let goalNode = Map.find positionM nodes
            let currentNode = Map.find currentPositionM nodes
            let navigationPathOpt =
                AStar.FindPath (
                    currentNode,
                    goalNode,
                    (fun n n2 -> if n2.PositionM.Y <> n.PositionM.Y then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                    (fun _ -> 0.0f))
            match navigationPathOpt with
            | null -> None
            | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

        static let walk3 positive current destination =
            let walkSpeed = if positive then Constants.Layout.CharacterWalkSpeed else -Constants.Layout.CharacterWalkSpeed
            let next = current + walkSpeed
            let delta = if positive then destination - next else next - destination
            if delta < Constants.Layout.CharacterWalkSpeed then (destination, WalkFinished) else (next, WalkContinuing)

        static let walk walkDescriptor (position : Vector2) =
            let walkOrigin = vmtovf walkDescriptor.WalkOriginM
            let walkVector = dtovf walkDescriptor.WalkDirection
            let walkDestination = walkOrigin + walkVector
            match walkDescriptor.WalkDirection with
            | Upward -> let (newY, arrival) = walk3 true position.Y walkDestination.Y in (Vector2 (position.X, newY), arrival)
            | Rightward -> let (newX, arrival) = walk3 true position.X walkDestination.X in (Vector2 (newX, position.Y), arrival)
            | Downward -> let (newY, arrival) = walk3 false position.Y walkDestination.Y in (Vector2 (position.X, newY), arrival)
            | Leftward -> let (newX, arrival) = walk3 false position.X walkDestination.X in (Vector2 (newX, position.Y), arrival)
        
        override this.Channel (_, _) =
            [//Simulants.Player.CharacterActivityState.ChangeEvent => cmd ToggleHaltButton
             Stream.make Simulants.HudFeeler.TouchEvent |> Stream.isSelected Simulants.HudFeeler =|> fun evt -> cmd (HandlePlayerInput (TouchInput evt.Data))
             Stream.make Simulants.HudDetailUp.DownEvent |> Stream.isSelected Simulants.HudDetailUp => cmd (HandlePlayerInput (DetailInput Upward))
             Stream.make Simulants.HudDetailRight.DownEvent |> Stream.isSelected Simulants.HudDetailRight => cmd (HandlePlayerInput (DetailInput Rightward))
             Stream.make Simulants.HudDetailDown.DownEvent |> Stream.isSelected Simulants.HudDetailDown => cmd (HandlePlayerInput (DetailInput Downward))
             Stream.make Simulants.HudDetailLeft.DownEvent |> Stream.isSelected Simulants.HudDetailLeft => cmd (HandlePlayerInput (DetailInput Leftward))
             Simulants.Gameplay.SelectEvent => msg StartGameplay
             Simulants.Gameplay.UpdateEvent => cmd Tick
             Simulants.Gameplay.PostUpdateEvent => cmd PostTick
             Simulants.HudSaveGame.ClickEvent => cmd SaveGame]

        override this.Message (model, message, _, world) =
            match message with
            
            | TryContinueNavigation ->
                let playerMoveOpt =
                    match model.Player.TurnStatus with
                    | TurnFinishing ->
                        match GameplayModel.getMultiMove PlayerIndex model with
                        | Travel (_ :: navigationPath) ->
                            let targetPositionM = (List.head navigationPath).PositionM
                            if List.exists (fun x -> x = targetPositionM) model.MoveModeler.AvailableCoordinates then
                                Some (MultiRoundMove (Travel navigationPath))
                            else None
                    | _ -> None
             
                let model =
                    match model.Player.TurnStatus with
                    | TurnFinishing -> GameplayModel.updateTurnStatus PlayerIndex Idle model
                    | _ -> model
                
                match playerMoveOpt with
                | Some move -> withMsg model (PlayNewRound move)
                | _ -> just model
            
            | FinishTurns indices ->
                let updater index model =
                    match (GameplayModel.getTurnStatus index model) with
                    | TurnFinishing ->
                        match GameplayModel.getCharacterActivityState index model with
                        | Action actionDescriptor ->
                            let model = GameplayModel.finishMove index model
                            let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                            let reactorState = GameplayModel.getCharacterState reactorIndex model
                            let characterAnimationState = GameplayModel.getCharacterAnimationState index model
                            let model = GameplayModel.updateCharacterAnimationState index (characterAnimationState.Facing (World.getTickTime world)) model
                            if reactorState.HitPoints <= 0 then
                                match reactorIndex with
                                | PlayerIndex -> GameplayModel.updateCharacterState reactorIndex {reactorState with ControlType = Uncontrolled} model // TODO: reimplement screen transition
                                | EnemyIndex _ -> GameplayModel.removeEnemy reactorIndex model
                            else model
                        | Navigation navigationDescriptor ->
                            let position = GameplayModel.getCoordinates index model |> vmtovf
                            let model = GameplayModel.updatePosition index position model
                            match navigationDescriptor.NavigationPathOpt with
                            | None
                            | Some (_ :: []) -> GameplayModel.finishMove index model
                            | _ -> model
                        | _ -> failwith "TurnStatus is TurnFinishing; CharacterActivityState should not be NoActivity"
                    | _ -> failwith "non-finishing turns should be filtered out by this point"

                let model = GameplayModel.forEachIndex updater indices model
                withMsg model TryContinueNavigation
            
            | ProgressTurns indices ->
                
                let updater index model =
                    match (GameplayModel.getTurnStatus index model) with
                    | TurnProgressing ->
                        match GameplayModel.getCharacterActivityState index model with
                        | Action actionDescriptor ->
                            let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                            let reactorState = GameplayModel.getCharacterState reactorIndex model
                            
                            let model = 
                                if actionDescriptor.ActionTicks = Constants.InfinityRpg.ReactionTick then
                                    if reactorState.HitPoints <= 0 then
                                        let reactorCharacterAnimationState = GameplayModel.getCharacterAnimationState reactorIndex model
                                        GameplayModel.updateCharacterAnimationState reactorIndex reactorCharacterAnimationState.Slain model
                                    else model
                                else model

                            if actionDescriptor.ActionTicks = Constants.InfinityRpg.ActionTicksMax then
                                GameplayModel.updateTurnStatus index TurnFinishing model
                            else GameplayModel.updateCharacterActivityState index (Action actionDescriptor.Inc) model
                        | Navigation navigationDescriptor ->
                            let (newPosition, walkState) = GameplayModel.getPosition index model |> walk navigationDescriptor.WalkDescriptor
                            let model = GameplayModel.updatePosition index newPosition model

                            match walkState with
                            | WalkFinished -> GameplayModel.updateTurnStatus index TurnFinishing model
                            | WalkContinuing -> model
                        | NoActivity -> failwith "TurnStatus is TurnProgressing; CharacterActivityState should not be NoActivity"
                    | _ -> model

                let model = GameplayModel.forEachIndex updater indices model
                let indices = List.filter (fun x -> (GameplayModel.getTurnStatus x model) = TurnFinishing) indices

                withMsg model (FinishTurns indices)
            
            | BeginTurns indices ->

                let updater index model =
                    let characterAnimationState = GameplayModel.getCharacterAnimationState index model
                    match (GameplayModel.getTurnStatus index model) with
                    | TurnBeginning ->
                        let characterAnimationState =
                            match (GameplayModel.getCharacterActivityState index model) with
                            | Action actionDescriptor ->
                                let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                                let characterPosition = GameplayModel.getPosition index model
                                let direction = ActionDescriptor.computeActionDirection characterPosition (GameplayModel.getCoordinates reactorIndex model)
                                CharacterAnimationState.makeAction (World.getTickTime world) direction
                            | Navigation navigationDescriptor ->
                                characterAnimationState.UpdateDirection navigationDescriptor.WalkDescriptor.WalkDirection
                            | _ -> failwith "TurnStatus is TurnBeginning; CharacterActivityState should not be NoActivity"
                        let model = GameplayModel.updateCharacterAnimationState index characterAnimationState model
                        GameplayModel.updateTurnStatus index TurnProgressing model // "TurnProgressing" for normal animation; "TurnFinishing" for roguelike mode
                    | _ -> model
                
                let model = GameplayModel.forEachIndex updater indices model
                
                withMsg model (ProgressTurns indices)
            
            | RunCharacterActivation ->
                
                // player's turn is converted to activity at the beginning of the round, activating the observable playback of his move
                let model =
                    if model.Player.TurnStatus = TurnPending then
                        GameplayModel.activateCharacter PlayerIndex model
                    else model

                // enemies are so activated at the same time during player movement, or after player's action has finished playback
                let indices = GameplayModel.getEnemyIndices model |> List.filter (fun x -> (GameplayModel.getTurnStatus x model) <> Idle)
                let model =
                    if (List.exists (fun x -> (GameplayModel.getTurnStatus x model) = TurnPending) indices) then
                        match model.Player.CharacterActivityState with
                        | Action _ -> model
                        | Navigation _ 
                        | NoActivity -> // TODO: find out why using activateCharacter here triggered "turn status is TurnBeginning..." exception
                            let enemyActivities = GameplayModel.getEnemyTurns model |> List.map Turn.toCharacterActivityState
                            let model = GameplayModel.forEachIndex (fun index model -> GameplayModel.updateTurnStatus index TurnBeginning model) indices model
                            GameplayModel.updateEnemyActivityStates enemyActivities model
                    else model
                                
                let indices = List.filter (fun x -> (GameplayModel.getTurnStatus x model) <> TurnPending) indices
                
                let indices =
                    match model.Player.TurnStatus with
                    | Idle -> indices
                    | _ -> PlayerIndex :: indices
                
                withMsg model (BeginTurns indices)
            
            | PlayNewRound playerMove ->
                
                // player makes his move
                
                let model = GameplayModel.addMove PlayerIndex playerMove model
                
                let model =
                    match playerMove with
                    | SingleRoundMove singleRoundMove ->
                        match singleRoundMove with
                        | Step direction -> GameplayModel.applyStep PlayerIndex direction model
                        | Attack reactorIndex -> GameplayModel.applyAttack reactorIndex model
                    | MultiRoundMove multiRoundMove ->
                        match multiRoundMove with
                        | Travel (head :: _) -> GameplayModel.relocateCharacter PlayerIndex head.PositionM model
                
                // (still standing) enemies make theirs
                
                let indices = GameplayModel.getEnemyIndices model
                let attackerOpt = List.tryFind (fun x -> Math.arePositionMsAdjacent (GameplayModel.getCoordinates x model) (GameplayModel.getCoordinates PlayerIndex model)) indices

                let updater =
                    (fun index model ->
                        let characterState = GameplayModel.getCharacterState index model
                        let enemyMoveOpt =
                            match characterState.ControlType with
                            | Chaos ->
                                if characterState.HitPoints > 0 then
                                    match attackerOpt with
                                    | Some attackerIndex ->
                                        if index = attackerIndex then
                                            Some (SingleRoundMove (Attack PlayerIndex))
                                        else None
                                    | None ->
                                        let openDirections = GameplayModel.getCoordinates index model |> model.MoveModeler.OpenDirections
                                        let direction = Gen.random1 4 |> Direction.fromInt
                                        if List.exists (fun x -> x = direction) openDirections then
                                            Some (SingleRoundMove (Step direction))
                                        else None
                                else None
                            | _ -> None
                        
                        match enemyMoveOpt with
                        | Some move ->
                            let model = GameplayModel.addMove index move model
                            match move with
                            | SingleRoundMove singleRoundMove ->
                                match singleRoundMove with
                                | Step direction -> GameplayModel.applyStep index direction model
                                | Attack reactorIndex ->
                                    let model = GameplayModel.applyAttack reactorIndex model
                                    GameplayModel.stopTraveler reactorIndex model
                            | _ -> failwith "enemy not yet capable of multiround move"
                        | None -> model)
                        
                let model = GameplayModel.forEachIndex updater indices model

                withMsg model RunCharacterActivation

            | TryMakePlayerMove playerInput ->

                let currentCoordinates = GameplayModel.getCoordinates PlayerIndex model
                
                let targetCoordinatesOpt =
                    match playerInput with
                    | TouchInput touchPosition -> Some (World.mouseToWorld false touchPosition world |> vftovm)
                    | DetailInput direction -> Some (currentCoordinates + dtovm direction)
                    | NoInput -> None

                let playerMoveOpt =
                    match targetCoordinatesOpt with
                    | Some coordinates ->
                        match Math.arePositionMsAdjacent coordinates currentCoordinates with
                        | true ->
                            let openDirections = model.MoveModeler.OpenDirections currentCoordinates
                            let direction = vmtod (coordinates - currentCoordinates)
                            let opponents = GameplayModel.getOpponentIndices PlayerIndex model
                            if List.exists (fun x -> x = direction) openDirections then
                                Some (SingleRoundMove (Step direction))
                            elif List.exists (fun index -> (GameplayModel.getCoordinates index model) = coordinates) opponents then
                                let targetIndex = GameplayModel.getIndexByCoordinates coordinates model
                                Some (SingleRoundMove (Attack targetIndex))
                            else None
                        | false ->
                            match tryGetNavigationPath PlayerIndex coordinates model with
                            | Some navigationPath ->
                                match navigationPath with
                                | [] -> None
                                | _ -> Some (MultiRoundMove (Travel navigationPath))
                            | None -> None
                    | None -> None
                
                match playerMoveOpt with
                | Some move -> withMsg model (PlayNewRound move)
                | _ -> just model

            | StartGameplay ->
                if model.ShallLoadGame && File.Exists Assets.SaveFilePath then
                    let modelStr = File.ReadAllText Assets.SaveFilePath
                    let model = scvalue<GameplayModel> modelStr
                    just model
                else
                    let fieldMap = Rand.makeFromSeedState model.ContentRandState |> createField
                    let model = GameplayModel.addFieldMap fieldMap model
                    let model = GameplayModel.makePlayer model
                    let model = GameplayModel.makeEnemies 9 model
                    just model

        override this.Command (model, command, _, world) =
            match command with
            | ToggleHaltButton ->
                let world = Simulants.HudHalt.SetEnabled (model.Player.CharacterActivityState.IsNavigatingPath) world
                just world
            | HandlePlayerInput playerInput ->
                if not (GameplayModel.anyTurnsInProgress model) then
                    let world = Simulants.HudSaveGame.SetEnabled false world
                    match model.Player.CharacterState.ControlType with
                    | PlayerControlled -> withMsg world (TryMakePlayerMove playerInput)
                    | _ -> just world
                else just world
            | SaveGame ->
                let modelStr = scstring model
                File.WriteAllText (Assets.SaveFilePath, modelStr)
                just world
            | Tick ->
                if (GameplayModel.anyTurnsInProgress model) then withMsg world RunCharacterActivation
                elif KeyboardState.isKeyDown KeyboardKey.Up then withCmd world (HandlePlayerInput (DetailInput Upward))
                elif KeyboardState.isKeyDown KeyboardKey.Right then withCmd world (HandlePlayerInput (DetailInput Rightward))
                elif KeyboardState.isKeyDown KeyboardKey.Down then withCmd world (HandlePlayerInput (DetailInput Downward))
                elif KeyboardState.isKeyDown KeyboardKey.Left then withCmd world (HandlePlayerInput (DetailInput Leftward))
                elif not (Simulants.HudSaveGame.GetEnabled world) then just (Simulants.HudSaveGame.SetEnabled true world)
                else just world
            | PostTick -> // Note: it appears a slight camera offset has been introduced with this code migration
                let eyeCenter = Simulants.Player.GetPosition world + Simulants.Player.GetSize world * 0.5f
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
                let world = World.setEyeCenter eyeCenter world
                just world
            | Nop -> just world

        override this.Content (model, screen) =
        
            [Content.layerIfScreenSelected screen (fun _ _ ->
                Content.layer Simulants.Scene.Name []

                    [Content.entityOpt model (fun model -> model.FieldMapOpt) (fun fieldMap world ->
                       let fieldMap = fieldMap.Get world
                       Content.entity<FieldDispatcher> Simulants.Field.Name
                           [Entity.FieldModel == { FieldMapNp = fieldMap }
                            Entity.Size == vmtovf fieldMap.FieldSizeM
                            Entity.Persistent == false])

                     Content.entitiesIndexedBy model
                        (fun model -> model.Enemies) constant
                        (fun model -> model.Index.getEnemyIndex)
                        (fun index model _ -> Content.entity<EnemyDispatcher> ("Enemy+" + scstring index) [Entity.CharacterModel <== model])

                     Content.entity<PlayerDispatcher> Simulants.Player.Name
                       [Entity.CharacterModel <== model --> fun model -> model.Player]])]
