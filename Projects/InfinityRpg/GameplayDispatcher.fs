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
        | Step
        | Attack

    type MultiRoundMove = // must be reducible to SingleRoundMoves e.g. Travel = { Step, Step ...}
        | Travel

    type Move =
        | SingleRoundMove of SingleRoundMove
        | MultiRoundMove of MultiRoundMove

    type MoveModeler =
        { PassableCoordinates : Vector2i list
          CharacterCoordinates : Map<CharacterIndex, Vector2i>
          LatestSingleMoves : Map<CharacterIndex, SingleRoundMove>
          CurrentMultiMoves : Map<CharacterIndex, MultiRoundMove> }

        static member empty =
            { PassableCoordinates = []
              CharacterCoordinates = Map.empty
              LatestSingleMoves = Map.empty
              CurrentMultiMoves = Map.empty }
    
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

        static member getCharacterOpponents index model =
            match index with
            | PlayerIndex -> model.Enemies
            | _ -> [model.Player]
        
        static member tryGetCharacterAtPositionM positionM model =
            GameplayModel.getCharacters model |> List.tryFind (fun model -> model.PositionM = positionM)

        static member getCharacterAtPositionM positionM model =
            GameplayModel.tryGetCharacterAtPositionM positionM model |> Option.get

        static member getCharacterIndexFromPositionM positionM model =
            (GameplayModel.getCharacterAtPositionM positionM model).Index
        
        static member getTurn index model =
            (GameplayModel.getCharacterByIndex index model).Turn

        static member getCharacterState index model =
            (GameplayModel.getCharacterByIndex index model).CharacterState
        
        static member getPositionM index model =
            (GameplayModel.getCharacterByIndex index model).PositionM
        
        static member getTurnStatus index model =
            (GameplayModel.getCharacterByIndex index model).TurnStatus
        
        static member getCharacterActivityState index model =
            (GameplayModel.getCharacterByIndex index model).CharacterActivityState

        static member getCharacterAnimationState index model =
            (GameplayModel.getCharacterByIndex index model).CharacterAnimationState
        
        static member getPosition index model =
            (GameplayModel.getCharacterByIndex index model).Position
        
        static member getCharacterIndices model =
            GameplayModel.getCharacters model |> List.map (fun model -> model.Index)
        
        static member getEnemyIndices model =
            List.map (fun model -> model.Index) model.Enemies
        
        static member getEnemyTurns model =
            List.map (fun model -> model.Turn) model.Enemies
        
        static member getCharacterPositionMs model =
            GameplayModel.getCharacters model |> List.map (fun model -> model.PositionM)

        static member getCharacterTurnStati model =
            GameplayModel.getCharacters model |> List.map (fun model -> model.TurnStatus)
        
        static member getEnemyActivityStates model =
            List.map (fun model -> model.CharacterActivityState) model.Enemies
        
        static member getCharacterActivityStates model =
            model.Player.CharacterActivityState :: (GameplayModel.getEnemyActivityStates model)
        
        static member getEnemyPositions model =
            List.map (fun model -> model.Position) model.Enemies
        
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
        
        static member updatePositionM index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updatePositionM index newValue model
        
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
            { model with Enemies = enemies }

        static member updateEnemiesBy updater newValues model =
            let enemies = List.map2 (fun newValue model -> updater newValue model) newValues model.Enemies
            { model with Enemies = enemies }

        static member updateEnemyActivityStates newValues model =
            GameplayModel.updateEnemiesBy CharacterModel.updateCharacterActivityState newValues model

        static member updateEnemyTurns newValues model =
            GameplayModel.updateEnemiesBy CharacterModel.updateTurn newValues model

        static member resetEnemyTurns model =
            let enemies = List.map (fun model -> CharacterModel.updateTurn NoTurn model) model.Enemies
            { model with Enemies = enemies }

        static member applyTurn index model =
            match (GameplayModel.getTurn index model) with
            | ActionTurn actionDescriptor ->
                let reactorDamage = 4 // NOTE: just hard-coding damage for now
                let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                let reactorTurn = GameplayModel.getTurn reactorIndex model
                let reactorState = GameplayModel.getCharacterState reactorIndex model
                let model = GameplayModel.updateCharacterState reactorIndex { reactorState with HitPoints = reactorState.HitPoints - reactorDamage } model
                match reactorTurn with
                | NavigationTurn navigationDescriptor ->
                    GameplayModel.updateTurn reactorIndex (NavigationTurn navigationDescriptor.CancelNavigation) model
                | _ -> model
            | NavigationTurn navigationDescriptor ->
                let characterModel = GameplayModel.getCharacterByIndex index model
                let positionM = characterModel.PositionM + dtovm navigationDescriptor.WalkDescriptor.WalkDirection
                GameplayModel.updatePositionM index positionM model
            | _ -> model

        static member occupationMap indexOpt model = // provide index for adjacent character map
            let fieldTiles = (Option.get model.FieldMapOpt).FieldTiles
            let characterPositions = GameplayModel.getCharacterPositionMs model |> List.map (fun positionM -> vmtovf positionM)
            match indexOpt with
            | Some index ->
                let characterPositionM = GameplayModel.getPositionM index model
                OccupationMap.makeFromFieldTilesAndAdjacentCharacters characterPositionM fieldTiles characterPositions
            | None -> OccupationMap.makeFromFieldTilesAndCharacters fieldTiles characterPositions

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
        | PlayNewRound of Turn
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

        static let createEnemies fieldMap =
            let randResult = Gen.random1 5
            let enemyCount = randResult + 5
            let (models, _) =
                List.fold
                    (fun (models, coords) index ->
                        let availableCoordinates = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles coords |> Map.filter (fun _ occupied -> occupied = false) |> Map.toKeyList |> List.toArray
                        let randResult = Gen.random1 availableCoordinates.Length
                        let enemyCoordinates = availableCoordinates.[randResult]
                        let model = CharacterModel.makeEnemy index enemyCoordinates
                        (model :: models, (vmtovf enemyCoordinates) :: coords))
                    ([], [])
                    [0 .. enemyCount - 1]
            models

        static let tryGetNavigationPath index positionM occupationMap model =
            let characterModel = GameplayModel.getCharacterByIndex index model
            let nodes = OccupationMap.makeNavigationNodes occupationMap
            let goalNode = Map.find positionM nodes
            let currentNode = Map.find characterModel.PositionM nodes
            let navigationPathOpt =
                AStar.FindPath (
                    currentNode,
                    goalNode,
                    (fun n n2 -> if n2.PositionM.Y <> n.PositionM.Y then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                    (fun _ -> 0.0f))
            match navigationPathOpt with
            | null -> None
            | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

        static let determineCharacterTurnFromPosition index targetPositionM occupationMap model =
            let currentPositionM = GameplayModel.getPositionM index model
            match Math.arePositionMsAdjacent targetPositionM currentPositionM with
            | true ->
                let openDirections = OccupationMap.getOpenDirectionsAtPositionM currentPositionM occupationMap
                let direction = vmtod (targetPositionM - currentPositionM)
                let opponents = GameplayModel.getCharacterOpponents index model
                if Set.contains direction openDirections then
                    Turn.makeNavigation None currentPositionM direction
                elif List.exists (fun opponent -> opponent.PositionM = targetPositionM) opponents then
                    GameplayModel.getCharacterIndexFromPositionM targetPositionM model |> Turn.makeAttack
                else NoTurn
            | false ->
                match tryGetNavigationPath index targetPositionM occupationMap model with
                | Some navigationPath ->
                    match navigationPath with
                    | [] -> NoTurn
                    | (head :: _) ->
                        if not (Map.find head.PositionM occupationMap) then
                            let walkDirection = vmtod (head.PositionM - currentPositionM)
                            Turn.makeNavigation (Some navigationPath) currentPositionM walkDirection
                        else CancelTurn // space became occupied
                | None -> NoTurn

        static let determineCharacterTurnFromDirection index direction occupationMap model =
            let positionM = GameplayModel.getPositionM index model
            let targetPositionM = positionM + dtovm direction
            determineCharacterTurnFromPosition index targetPositionM occupationMap model
        
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
                let currentPositionM = model.Player.PositionM
                let playerTurn =
                    if model.Player.Position = vmtovf currentPositionM then
                        match model.Player.CharacterActivityState with
                        | Navigation navigationDescriptor ->
                            match navigationDescriptor.NavigationPathOpt with
                            | Some (currentNode :: navigationPath) ->
                                let targetPositionM = (List.head navigationPath).PositionM
                                let occupationMap = GameplayModel.occupationMap None model
                                if not (Map.find targetPositionM occupationMap) then
                                    let walkDirection = vmtod (targetPositionM - currentPositionM)
                                    let navigationDescriptor = NavigationDescriptor.make (Some navigationPath) currentPositionM walkDirection
                                    NavigationTurn navigationDescriptor
                                else CancelTurn
                            | _ -> failwith "at this point navigation path should exist with length > 1"
                        | _ -> NoTurn
                    else NoTurn

                let model =
                    if playerTurn = CancelTurn then
                        GameplayModel.updateTurnStatus PlayerIndex Idle model
                    else model
                
                match playerTurn with
                | ActionTurn _
                | NavigationTurn _ -> withMsg model (PlayNewRound playerTurn)
                | _ -> just model
            
            | FinishTurns indices ->
                let updater index model =
                    match (GameplayModel.getTurnStatus index model) with
                    | TurnFinishing ->
                        match GameplayModel.getCharacterActivityState index model with
                        | Action actionDescriptor ->
                            let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                            let reactorState = GameplayModel.getCharacterState reactorIndex model
                            let characterAnimationState = GameplayModel.getCharacterAnimationState index model

                            let model = GameplayModel.updateTurnStatus index Idle model
                            let model = GameplayModel.updateCharacterActivityState index NoActivity model
                            let model = GameplayModel.updateCharacterAnimationState index (characterAnimationState.Facing (World.getTickTime world)) model
                            if reactorState.HitPoints <= 0 then
                                match reactorIndex with
                                | PlayerIndex -> GameplayModel.updateCharacterState reactorIndex {reactorState with ControlType = Uncontrolled} model // TODO: reimplement screen transition
                                | EnemyIndex _ -> GameplayModel.removeEnemy reactorIndex model
                            else model
                        | Navigation navigationDescriptor ->
                            let position = GameplayModel.getPositionM index model |> vmtovf
                            let model = GameplayModel.updatePosition index position model
                            match navigationDescriptor.NavigationPathOpt with
                            | None
                            | Some (_ :: []) ->
                                let model = GameplayModel.updateCharacterActivityState index NoActivity model
                                GameplayModel.updateTurnStatus index Idle model
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
                                let direction = ActionDescriptor.computeActionDirection characterPosition (GameplayModel.getPositionM reactorIndex model)
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
                        let activity = Turn.toCharacterActivityState model.Player.Turn
                        let model = GameplayModel.updateTurnStatus PlayerIndex TurnBeginning model
                        let model = GameplayModel.updateCharacterActivityState PlayerIndex activity model
                        GameplayModel.updateTurn PlayerIndex NoTurn model
                    else model

                // enemies are so activated at the same time during player movement, or after player's action has finished playback
                let indices = GameplayModel.getEnemyIndices model |> List.filter (fun x -> (GameplayModel.getTurnStatus x model) <> Idle)
                let model =
                    if (List.exists (fun x -> (GameplayModel.getTurnStatus x model) = TurnPending) indices) then
                        match model.Player.CharacterActivityState with
                        | Action _ -> model
                        | Navigation _ 
                        | NoActivity ->
                            let enemyActivities = GameplayModel.getEnemyTurns model |> List.map Turn.toCharacterActivityState
                            let model = GameplayModel.forEachIndex (fun index model -> GameplayModel.updateTurnStatus index TurnBeginning model) indices model
                            let model = GameplayModel.updateEnemyActivityStates enemyActivities model
                            GameplayModel.resetEnemyTurns model
                    else model
                                
                let indices = List.filter (fun x -> (GameplayModel.getTurnStatus x model) <> TurnPending) indices
                
                let indices =
                    match model.Player.TurnStatus with
                    | Idle -> indices
                    | _ -> PlayerIndex :: indices
                
                withMsg model (BeginTurns indices)
            
            | PlayNewRound newPlayerTurn ->
                
                // player makes his move
                let model = GameplayModel.updateTurn PlayerIndex newPlayerTurn model
                let model = GameplayModel.updateTurnStatus PlayerIndex TurnPending model
                let model = GameplayModel.applyTurn PlayerIndex model
                
                // (still standing) enemies make theirs
                let turnGenerator index (occupationMap, enemyTurns) =
                    let enemyPositionM = GameplayModel.getPositionM index model
                    let characterState = GameplayModel.getCharacterState index model
                    let enemyTurn =
                        match characterState.ControlType with
                        | Chaos ->
                            if characterState.HitPoints <= 0 then NoTurn
                            else
                                if Math.arePositionMsAdjacent enemyPositionM model.Player.PositionM then
                                    Turn.makeAttack PlayerIndex
                                else
                                    let randResult = Gen.random1 4
                                    let direction = Direction.fromInt randResult
                                    determineCharacterTurnFromDirection index direction occupationMap model
                        | _ -> NoTurn
                    let occupationMap = OccupationMap.transferByDesiredTurn enemyTurn (vmtovf enemyPositionM) occupationMap
                    (occupationMap, enemyTurn :: enemyTurns)
                
                // if any action turn is present, all actions except the first are cancelled
                let turnFilter enemyTurns =
                    if List.exists Turn.isAction enemyTurns then
                        let firstActionIndex = List.tryFindIndex Turn.isAction enemyTurns |> Option.get
                        List.mapi (fun index (turn : Turn) -> if index = firstActionIndex then turn else NoTurn ) enemyTurns
                    else enemyTurns
                
                let indices = GameplayModel.getEnemyIndices model
                let enemyTurns = List.foldBack turnGenerator indices (GameplayModel.occupationMap None model, []) |> snd |> turnFilter

                let model = GameplayModel.updateEnemyTurns enemyTurns model
                let model = GameplayModel.forEachIndex (fun index model -> if Turn.toCharacterActivityState (GameplayModel.getTurn index model) <> NoActivity then GameplayModel.updateTurnStatus index TurnPending model else model) indices model
                let model = GameplayModel.forEachIndex GameplayModel.applyTurn indices model
                
                withMsg model RunCharacterActivation

            | TryMakePlayerMove playerInput ->

                let occupationMap = GameplayModel.occupationMap (Some PlayerIndex) model
            
                let playerTurn =
                    match playerInput with
                    | TouchInput touchPosition ->
                        let targetPositionM = World.mouseToWorld false touchPosition world |> vftovm
                        determineCharacterTurnFromPosition PlayerIndex targetPositionM occupationMap model
                    | DetailInput direction -> determineCharacterTurnFromDirection PlayerIndex direction occupationMap model
                    | NoInput -> NoTurn

                match playerTurn with
                | ActionTurn _
                | NavigationTurn _ -> withMsg model (PlayNewRound playerTurn)
                | _ -> just model

            | StartGameplay ->
                if model.ShallLoadGame && File.Exists Assets.SaveFilePath then
                    let modelStr = File.ReadAllText Assets.SaveFilePath
                    let model = scvalue<GameplayModel> modelStr
                    just model
                else
                    let rand = Rand.makeFromSeedState model.ContentRandState
                    let fieldMap = createField rand
                    let enemies = createEnemies fieldMap
                    let model = { model with FieldMapOpt = Some fieldMap; Enemies = enemies; Player = CharacterModel.makePlayer }
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

                     Content.entity<PlayerDispatcher> Simulants.Player.Name // TODO: didn't realise enemies' possible placements included outermost tiles allowing player/enemy overlap. another problem to deal with once structure is under control
                       [Entity.CharacterModel <== model --> fun model -> model.Player]])]
