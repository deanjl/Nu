namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module GameplayDispatcherModule =

    type [<StructuralEquality; NoComparison>] GameplayModel =
        { ContentRandState : uint64
          ShallLoadGame : bool
          FieldMapOpt : FieldMap option
          Enemies : CharacterModel list
          Player : CharacterModel }

        static member initial =
            let sysrandom = System.Random ()
            let contentSeedState = uint64 (sysrandom.Next ())
            { ContentRandState = contentSeedState
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
        
        static member getCharacterPositionM index model =
            (GameplayModel.getCharacterByIndex index model).PositionM
        
        static member getCharacterIndices model =
            GameplayModel.getCharacters model |> List.map (fun model -> model.Index)
        
        static member getEnemyIndices model =
            List.map (fun model -> model.Index) model.Enemies
        
        static member getEnemyPositions model =
            List.map (fun model -> model.Position) model.Enemies

        static member getCharacterPositionMs model =
            GameplayModel.getCharacters model |> List.map (fun model -> model.PositionM)

        static member getEnemyActivityStates model =
            List.map (fun model -> model.CharacterActivityState) model.Enemies
        
        static member getCharacterActivityStates model =
            model.Player.CharacterActivityState :: (GameplayModel.getEnemyActivityStates model)
        
        static member getEnemyTurns model =
            List.map (fun model -> model.Turn) model.Enemies

        static member enemyTurnsPending model =
            GameplayModel.getEnemyTurns model |> List.exists (fun turn -> turn <> NoTurn)
        
        static member anyTurnsInProgress model =
            GameplayModel.getCharacterActivityStates model |> List.exists (fun state -> state <> NoActivity) ||
            GameplayModel.enemyTurnsPending model
        
        static member updateCharacterBy updater index newValue model =
            match index with
            | PlayerIndex -> { model with Player = updater newValue model.Player }
            | EnemyIndex i as index ->
                let enemies =
                    model.Enemies |>
                    List.map (fun model -> if model.Index = index then updater newValue model else model)
                { model with Enemies = enemies }
        
        static member updatePosition index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updatePosition index newValue model

        static member updatePositionM index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updatePositionM index newValue model
        
        static member updateCharacterActivityState index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateCharacterActivityState index newValue model

        static member updateCharacterState index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateCharacterState index newValue model

        static member updateCharacterAnimationState index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateCharacterAnimationState index newValue model

        static member updateTurn index newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateTurn index newValue model

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

        static member applyTurn index turn model =
            match turn with
            | ActionTurn actionDescriptor ->
                let reactorDamage = 4 // NOTE: just hard-coding damage for now
                let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                let reactorModel = GameplayModel.getCharacterByIndex reactorIndex model
                let reactorState = reactorModel.CharacterState
                GameplayModel.updateCharacterState reactorIndex { reactorState with HitPoints = reactorState.HitPoints - reactorDamage } model
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
                let characterPositionM = GameplayModel.getCharacterPositionM index model
                OccupationMap.makeFromFieldTilesAndAdjacentCharacters characterPositionM fieldTiles characterPositions
            | None -> OccupationMap.makeFromFieldTilesAndCharacters fieldTiles characterPositions
        
    type [<StructuralEquality; NoComparison>] PlayerInput =
        | TouchInput of Vector2
        | DetailInput of Direction
        | NoInput

    type [<StructuralEquality; NoComparison>] GameplayMessage =
        | NewGame
        | UpdateActiveCharacters
        | RunCharacterActivation
        | PlayNewRound of Turn
        | TryMakePlayerMove of PlayerInput
    
    type [<NoEquality; NoComparison>] GameplayCommand =
        | ToggleHaltButton // TODO: reimplement once game is properly elmified
        | HandlePlayerInput of PlayerInput
        | SaveGame of Screen
        | QuitGameplay
        | RunGameplay
        | Tick
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

        static let getCharacterAnimationStateByActionBegin tickTime characterPosition characterAnimationState (actionDescriptor : ActionDescriptor) model =
            let targetIndex = Option.get actionDescriptor.ActionTargetIndexOpt
            let targetPositionM = (GameplayModel.getCharacterByIndex targetIndex model).PositionM
            let direction = actionDescriptor.ComputeActionDirection characterPosition targetPositionM
            { characterAnimationState with
                Direction = direction
                AnimationType = CharacterAnimationActing
                StartTime = tickTime }

        static let getCharacterAnimationStateByActionEnd tickTime characterAnimationState =
            { characterAnimationState with
                AnimationType = CharacterAnimationFacing
                StartTime = tickTime }

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

        static let isPlayerNavigatingPath model =
            model.Player.CharacterActivityState.IsNavigatingPath

        static let cancelNavigation index model =
            let characterModel = GameplayModel.getCharacterByIndex index model
            let characterActivity =
                match characterModel.CharacterActivityState with
                | Action _ as action -> action
                | NoActivity -> NoActivity
                | Navigation navDescriptor -> Navigation { navDescriptor with NavigationPathOpt = None }
            GameplayModel.updateCharacterActivityState index characterActivity model

        static let determineCharacterTurnFromPosition index targetPositionM occupationMap model =
            let currentPositionM = GameplayModel.getCharacterPositionM index model
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
                        if Map.find head.PositionM occupationMap then CancelTurn // rationale for this unclear; depends on AStar behavior
                        else
                            let walkDirection = vmtod (head.PositionM - currentPositionM)
                            Turn.makeNavigation (Some navigationPath) currentPositionM walkDirection
                | None -> NoTurn

        static let determineCharacterTurnFromDirection index direction occupationMap model =
            let positionM = GameplayModel.getCharacterPositionM index model
            let targetPositionM = positionM + dtovm direction
            determineCharacterTurnFromPosition index targetPositionM occupationMap model
        
        static let determinePlayerTurnFromNavigationProgress model =
            match model.Player.CharacterActivityState with
            | Action _ -> NoTurn
            | Navigation navigationDescriptor ->
                let walkDescriptor = navigationDescriptor.WalkDescriptor
                if model.Player.Position = vmtovf model.Player.PositionM then
                    let occupationMap = GameplayModel.occupationMap None model
                    let walkDestinationM = walkDescriptor.WalkOriginM + dtovm walkDescriptor.WalkDirection
                    if Map.find walkDestinationM occupationMap then CancelTurn
                    else NavigationTurn navigationDescriptor
                else NoTurn
            | NoActivity -> NoTurn

        static let determineEnemyTurn index occupationMap model =
            let characterModel = GameplayModel.getCharacterByIndex index model
            match characterModel.CharacterState.ControlType with
            | PlayerControlled as controlType ->
                Log.debug ("Invalid ControlType '" + scstring controlType + "' for enemy.")
                NoTurn
            | Chaos ->
                if characterModel.CharacterState.HitPoints <= 0 then NoTurn
                else
                    if Math.arePositionMsAdjacent characterModel.PositionM model.Player.PositionM then
                        Turn.makeAttack PlayerIndex
                    else
                        let randResult = Gen.random1 4
                        let direction = Direction.fromInt randResult
                        determineCharacterTurnFromDirection index direction occupationMap model
            | Uncontrolled -> NoTurn

        static let determineEnemyTurns occupationMap model =
            let (_, enemyTurns, _) =
                List.foldBack
                    (fun enemy (occupationMap, enemyTurns, model) ->
                        let enemyTurn = determineEnemyTurn enemy.Index occupationMap model
                        let occupationMap = OccupationMap.transferByDesiredTurn enemyTurn enemy.Position occupationMap
                        (occupationMap, enemyTurn :: enemyTurns, model))
                    (model.Enemies)
                    (occupationMap, [], model)
            
            // if any action turn is present, all actions except the first are cancelled
            let desiredEnemyAction = List.exists (fun (turn : Turn) -> turn.IsAction) enemyTurns
            if desiredEnemyAction then
                let firstActionIndex = List.tryFindIndex (fun (turn : Turn) -> match turn with ActionTurn _ -> true; | _ -> false) enemyTurns |> Option.get
                List.mapi (fun index (turn : Turn) -> if index = firstActionIndex then turn else NoTurn ) enemyTurns
            else enemyTurns

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
        
        static let tickNavigation index navigationDescriptor model =
            let characterModel = GameplayModel.getCharacterByIndex index model
            let walkDescriptor = navigationDescriptor.WalkDescriptor
            let (newPosition, walkState) = walk walkDescriptor characterModel.Position
            let characterAnimationState = { characterModel.CharacterAnimationState with Direction = walkDescriptor.WalkDirection }
            let model = GameplayModel.updateCharacterAnimationState index characterAnimationState model
            let model = GameplayModel.updatePosition index newPosition model

            match walkState with
            | WalkFinished ->
                match navigationDescriptor.NavigationPathOpt with
                | Some [] -> failwith "NavigationPath should never be empty here."
                | Some (_ :: []) ->
                    GameplayModel.updateCharacterActivityState index NoActivity model
                | Some (currentNode :: navigationPath) ->
                    let characterModel = GameplayModel.getCharacterByIndex index model
                    let walkDirection = vmtod ((List.head navigationPath).PositionM - currentNode.PositionM)
                    let navigationDescriptor = NavigationDescriptor.make (Some navigationPath) characterModel.PositionM walkDirection
                    GameplayModel.updateCharacterActivityState index (Navigation navigationDescriptor) model
                | None ->
                    GameplayModel.updateCharacterActivityState index NoActivity model
            | WalkContinuing -> model

        static let tickAction time index actionDescriptor model =
            let characterModel = GameplayModel.getCharacterByIndex index model
            let actionTicks = actionDescriptor.ActionTicks
            let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
            let reactorModel = GameplayModel.getCharacterByIndex reactorIndex model
            let reactorState = reactorModel.CharacterState
            let characterAnimationState = characterModel.CharacterAnimationState
            let actionInc = Action { actionDescriptor with ActionTicks = inc actionTicks }

            if actionTicks = 0L then
                let newAnimation = getCharacterAnimationStateByActionBegin time characterModel.Position characterAnimationState actionDescriptor model
                let model = GameplayModel.updateCharacterAnimationState index newAnimation model
                GameplayModel.updateCharacterActivityState index actionInc model
            elif actionTicks = (Constants.InfinityRpg.CharacterAnimationActingDelay * 2L) then
                let model = GameplayModel.updateCharacterActivityState index actionInc model
                if reactorState.HitPoints <= 0 then
                    let characterAnimationState = reactorModel.CharacterAnimationState
                    GameplayModel.updateCharacterAnimationState reactorIndex { characterAnimationState with AnimationType = CharacterAnimationSlain } model
                else model
            elif actionTicks = Constants.InfinityRpg.ActionTicksMax then
                let newAnimation = getCharacterAnimationStateByActionEnd time characterAnimationState
                let model = GameplayModel.updateCharacterAnimationState index newAnimation model
                let model = GameplayModel.updateCharacterActivityState index NoActivity model
                if reactorState.HitPoints <= 0 then
                    match reactorIndex with
                    | PlayerIndex -> GameplayModel.updateCharacterState reactorIndex {reactorState with ControlType = Uncontrolled} model // TODO: reimplement screen transition
                    | EnemyIndex _ -> GameplayModel.removeEnemy reactorIndex model
                else model
            else GameplayModel.updateCharacterActivityState index actionInc model
        
        override this.Channel (_, _) =
            [//Simulants.Player.CharacterActivityState.ChangeEvent => cmd ToggleHaltButton
             Stream.make Simulants.HudFeeler.TouchEvent |> Stream.isSelected Simulants.HudFeeler =|> fun evt -> cmd (HandlePlayerInput (TouchInput evt.Data))
             Stream.make Simulants.HudDetailUp.DownEvent |> Stream.isSelected Simulants.HudDetailUp => cmd (HandlePlayerInput (DetailInput Upward))
             Stream.make Simulants.HudDetailRight.DownEvent |> Stream.isSelected Simulants.HudDetailRight => cmd (HandlePlayerInput (DetailInput Rightward))
             Stream.make Simulants.HudDetailDown.DownEvent |> Stream.isSelected Simulants.HudDetailDown => cmd (HandlePlayerInput (DetailInput Downward))
             Stream.make Simulants.HudDetailLeft.DownEvent |> Stream.isSelected Simulants.HudDetailLeft => cmd (HandlePlayerInput (DetailInput Leftward))
             Simulants.Gameplay.UpdateEvent => cmd Tick
             Simulants.Gameplay.SelectEvent => cmd RunGameplay
             Simulants.HudSaveGame.ClickEvent =|> fun evt -> cmd (SaveGame evt.Subscriber)
             Simulants.Gameplay.DeselectEvent => cmd QuitGameplay]

        override this.Message (model, message, _, world) =
            match message with
            | NewGame ->
                // TODO : reimplement and fix game loading
                // TODO : arrange so new game can be started more than once!
                
                // make rand from gameplay
                let rand = Rand.makeFromSeedState model.ContentRandState

                // make field
                let fieldMap = createField rand

                // make enemies
                let enemies = createEnemies fieldMap

                let model = { model with FieldMapOpt = Some fieldMap; Enemies = enemies; Player = CharacterModel.makePlayer }
                just model
            
            | UpdateActiveCharacters ->
                let time = World.getTickTime world
                
                let rec recursion (characters : CharacterIndex list) model =
                    if characters.Length = 0 then model
                    else
                        let index = characters.Head
                        let characterModelOpt = GameplayModel.tryGetCharacterByIndex index model
                        let model =
                            match characterModelOpt with
                            | None -> model
                            | Some characterModel ->
                                match characterModel.CharacterActivityState with
                                | Action actionDescriptor -> tickAction time index actionDescriptor model
                                | Navigation navigationDescriptor -> tickNavigation index navigationDescriptor model
                                | NoActivity -> model
                        recursion characters.Tail model
                let model = recursion (GameplayModel.getCharacterIndices model) model

                let playerTurn = determinePlayerTurnFromNavigationProgress model
                match playerTurn with
                | NoTurn -> just model
                | _ -> withMsg model (PlayNewRound playerTurn)
            
            | RunCharacterActivation ->
                
                let model =
                    if model.Player.Turn = NoTurn then model
                    else
                        let activity = Turn.toCharacterActivityState model.Player.Turn
                        let model = GameplayModel.updateCharacterActivityState PlayerIndex activity model
                        GameplayModel.updateTurn PlayerIndex NoTurn model

                (* set enemy activities in accordance with the player's current activity.
                enemy turns must await player action to finish before executing. *)
                
                let model =
                    if (GameplayModel.enemyTurnsPending model) then
                        match model.Player.CharacterActivityState with
                        | Action _ -> model
                        | Navigation _ 
                        | NoActivity ->
                            let enemyActivities = GameplayModel.getEnemyTurns model |> List.map Turn.toCharacterActivityState
                            let model = GameplayModel.updateEnemyActivityStates enemyActivities model
                            let model = GameplayModel.resetEnemyTurns model
                                
                            if List.exists (fun (state : CharacterActivityState) -> state.IsActing) (GameplayModel.getEnemyActivityStates model) then
                                cancelNavigation PlayerIndex model
                            else model
                    else model
                                
                withMsg model UpdateActiveCharacters                
            
            | PlayNewRound newPlayerTurn ->
                
                // player makes his move
                let model = GameplayModel.updateTurn PlayerIndex newPlayerTurn model
                let model = GameplayModel.applyTurn PlayerIndex newPlayerTurn model
                
                // (still standing) enemies make theirs
                let occupationMap = GameplayModel.occupationMap None model
                
                let enemyTurns = determineEnemyTurns occupationMap model
                let model = GameplayModel.updateEnemyTurns enemyTurns model

                let rec recursion (enemies : CharacterIndex list) model =
                    if enemies.Length = 0 then model
                    else
                        let index = enemies.Head
                        let enemyModel = GameplayModel.getCharacterByIndex index model
                        let model = GameplayModel.applyTurn index enemyModel.Turn model
                        recursion enemies.Tail model
                let model = recursion (GameplayModel.getEnemyIndices model) model
                
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

        override this.Command (model, command, _, world) =
            match command with
            | ToggleHaltButton ->
                let world = Simulants.HudHalt.SetEnabled (isPlayerNavigatingPath model) world
                just world
            | HandlePlayerInput playerInput ->
                if (GameplayModel.anyTurnsInProgress model) then just world
                else
                    let world = Simulants.HudSaveGame.SetEnabled false world
                    match model.Player.CharacterState.ControlType with
                    | PlayerControlled -> withMsg world (TryMakePlayerMove playerInput)
                    | _ -> just world
            | SaveGame gameplay ->
                World.writeScreenToFile Assets.SaveFilePath gameplay world
                just world
            | QuitGameplay ->
                let world = World.destroyLayer Simulants.Scene world
                just world
            | RunGameplay -> // Note: kept here to handle game loading once that is re-implemented
                withMsg world NewGame
            | Tick ->
                if (GameplayModel.anyTurnsInProgress model) then withMsg world RunCharacterActivation
                elif KeyboardState.isKeyDown KeyboardKey.Up then withCmd world (HandlePlayerInput (DetailInput Upward))
                elif KeyboardState.isKeyDown KeyboardKey.Right then withCmd world (HandlePlayerInput (DetailInput Rightward))
                elif KeyboardState.isKeyDown KeyboardKey.Down then withCmd world (HandlePlayerInput (DetailInput Downward))
                elif KeyboardState.isKeyDown KeyboardKey.Left then withCmd world (HandlePlayerInput (DetailInput Leftward))
                elif not (Simulants.HudSaveGame.GetEnabled world) then just (Simulants.HudSaveGame.SetEnabled true world)
                else just world
            | Nop -> just world

        override this.Content (model, _) =

            [Content.layer Simulants.Scene.Name []

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
                    [Entity.CharacterModel <== model --> fun model -> model.Player]]]
