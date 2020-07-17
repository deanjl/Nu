namespace InfinityRpg
open System
open System.IO
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

        static member getCharacterByIndex indexOpt model =
            match indexOpt with
            | None -> model.Player
            | Some index -> model.Enemies |> List.find (fun model -> (Option.get model.EnemyIndexOpt) = index)

        static member getCharacterOpponents indexOpt model =
            match indexOpt with
            | None -> model.Enemies
            | _ -> [model.Player]
        
        static member getCharacters model =
            model.Player :: model.Enemies

        static member tryGetCharacterAtPosition position model =
            GameplayModel.getCharacters model |> List.tryFind (fun model -> model.Position = position)

        static member getCharacterInDirection indexOpt direction model =
            let position = (GameplayModel.getCharacterByIndex indexOpt model).Position
            GameplayModel.tryGetCharacterAtPosition (position + dtovf direction) model |> Option.get
        
        static member getEnemyPositions model =
            List.map (fun model -> model.Position) model.Enemies
        
        static member getEnemyDesiredTurns model =
            List.map (fun model -> Option.get model.DesiredTurnOpt) model.Enemies
        
        static member updateCharacterBy updater indexOpt newValue model =
            match indexOpt with
            | None -> { model with Player = updater newValue model.Player }
            | Some index ->
                let enemies =
                    model.Enemies |>
                    List.map (fun model -> if (Option.get model.EnemyIndexOpt) = index then updater newValue model else model)
                { model with Enemies = enemies }
        
        static member updatePosition indexOpt newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updatePosition indexOpt newValue model

        static member updateCharacterActivityState indexOpt newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateCharacterActivityState indexOpt newValue model

        static member updateCharacterState indexOpt newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateCharacterState indexOpt newValue model

        static member updateCharacterAnimationState indexOpt newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateCharacterAnimationState indexOpt newValue model

        static member updateDesiredTurnOpt indexOpt newValue model =
            GameplayModel.updateCharacterBy CharacterModel.updateDesiredTurnOpt indexOpt newValue model

        static member removeEnemy index model =
            let enemies = List.filter (fun model -> (Option.get model.EnemyIndexOpt) <> index) model.Enemies
            { model with Enemies = enemies }
    
    type [<StructuralEquality; NoComparison>] PlayerInput =
        | TouchInput of Vector2
        | DetailInput of Direction
        | NoInput

    type GameplayMessage =
        | NewGame
    
    type [<NoEquality; NoComparison>] GameplayCommand =
        | ToggleHaltButton // TODO: reimplement once game is properly elmified
        | HandlePlayerInput of PlayerInput
        | SaveGame of Screen
        | QuittingGameplay
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

        static let getEnemies world =
            let entities = World.getEntities Simulants.Scene world
            Seq.filter (fun (entity : Entity) -> entity.Is<EnemyDispatcher> world) entities

        static let writeCharactersToGameplay model world =
            let enemies = getEnemies world |> List.ofSeq |> List.map (fun enemy -> enemy.GetCharacterModel world)
            let enemies = model.Enemies |> List.map (fun enemy -> List.tryFind (fun model -> (Option.get model.EnemyIndexOpt) = (Option.get enemy.EnemyIndexOpt) ) enemies) |> List.definitize
            let player = Simulants.Player.GetCharacterModel world
            { model with Enemies = enemies; Player = player }
        
        static let makeAttackTurn targetPositionM =
            ActionTurn
                { ActionTicks = 0L
                  ActionTargetPositionMOpt = Some targetPositionM
                  ActionDataName = Constants.InfinityRpg.AttackName }

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
            let rand = snd fieldTilesTupleList.Head
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
                        let enemyCoordinates = vmtovf availableCoordinates.[randResult]
                        let model =
                            { Position = enemyCoordinates
                              EnemyIndexOpt = Some index
                              CharacterActivityState = NoActivity
                              CharacterState = { CharacterState.empty with HitPoints = 10; ControlType = Chaos }
                              CharacterAnimationState = { StartTime = 0L; AnimationType = CharacterAnimationFacing; Direction = Upward }
                              CharacterAnimationSheet = Assets.GoopyImage
                              DesiredTurnOpt = Some NoTurn }
                        (model :: models, enemyCoordinates :: coords))
                    ([], [])
                    [0 .. enemyCount - 1]
            models

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

        static let getCharacterAnimationStateByActionBegin tickTime characterPosition characterAnimationState (actionDescriptor : ActionDescriptor) =
            let currentDirection = characterAnimationState.Direction
            let direction = actionDescriptor.ComputeActionDirection characterPosition currentDirection
            { characterAnimationState with
                Direction = direction
                AnimationType = CharacterAnimationActing
                StartTime = tickTime }

        static let getCharacterAnimationStateByActionEnd tickTime characterAnimationState =
            { characterAnimationState with
                AnimationType = CharacterAnimationFacing
                StartTime = tickTime }

        static let tryGetNavigationPath touchPosition occupationMap (character : Entity) world =
            let nodes = OccupationMap.makeNavigationNodes occupationMap
            let goalNode = Map.find (vftovm touchPosition) nodes
            let currentNode = Map.find (vftovm (character.GetPosition world)) nodes
            let navigationPathOpt =
                AStar.FindPath (
                    currentNode,
                    goalNode,
                    (fun n n2 -> if n2.PositionM.Y <> n.PositionM.Y then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                    (fun _ -> 0.0f))
            match navigationPathOpt with
            | null -> None
            | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

        static let isPlayerNavigatingPath world =
            (Simulants.Player.GetCharacterModel world).CharacterActivityState.IsNavigatingPath

        static let cancelNavigation indexOpt model =
            let characterModel = GameplayModel.getCharacterByIndex indexOpt model
            let characterActivity =
                match characterModel.CharacterActivityState with
                | Action _ as action -> action
                | NoActivity -> NoActivity
                | Navigation navDescriptor -> Navigation { navDescriptor with NavigationPathOpt = None }
            GameplayModel.updateCharacterActivityState characterModel.EnemyIndexOpt characterActivity model

        static let anyTurnsInProgress2 (player : Entity) enemies world =
            (player.GetCharacterModel world).CharacterActivityState <> NoActivity ||
            Seq.exists
                (fun (enemy : Entity) -> (enemy.GetCharacterModel world).DesiredTurnOpt <> Some NoTurn || (enemy.GetCharacterModel world).CharacterActivityState <> NoActivity)
                enemies

        static let anyTurnsInProgress world =
            let enemies = getEnemies world
            anyTurnsInProgress2 Simulants.Player enemies world

        static let determineCharacterTurnFromDirection indexOpt direction occupationMap model =
            let characterModel = GameplayModel.getCharacterByIndex indexOpt model
            match characterModel.CharacterActivityState with
            | Action _ -> NoTurn
            | Navigation _ -> NoTurn
            | NoActivity ->
                let characterPositionM = vftovm characterModel.Position
                let openDirections = OccupationMap.getOpenDirectionsAtPositionM characterPositionM occupationMap
                if Set.contains direction openDirections then
                    let walkDescriptor = { WalkDirection = direction; WalkOriginM = characterPositionM }
                    NavigationTurn { WalkDescriptor = walkDescriptor; NavigationPathOpt = None; LastWalkOriginM = characterPositionM }
                else
                    let targetPosition = characterModel.Position + dtovf direction
                    let opponents = GameplayModel.getCharacterOpponents indexOpt model
                    if List.exists (fun opponent -> opponent.Position = targetPosition) opponents
                    then makeAttackTurn (vftovm targetPosition)
                    else NoTurn

        static let determineCharacterTurnFromTouch touchPosition occupationMap (character : Entity) opponents model world =
            if (character.GetCharacterModel world).CharacterActivityState = NoActivity then
                match tryGetNavigationPath touchPosition occupationMap character world with
                | Some navigationPath ->
                    match navigationPath with
                    | [] -> NoTurn
                    | _ ->
                        let characterPositionM = vftovm (character.GetPosition world)
                        let walkDirection = vmtod ((List.head navigationPath).PositionM - characterPositionM)
                        let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = characterPositionM }
                        NavigationTurn { WalkDescriptor = walkDescriptor; NavigationPathOpt = Some navigationPath; LastWalkOriginM = characterPositionM }
                | None ->
                    let targetPosition = touchPosition |> vftovm |> vmtovf
                    if Math.arePositionsAdjacent targetPosition (character.GetPosition world) then
                        if Seq.exists (fun (opponent : Entity) -> opponent.GetPosition world = targetPosition) opponents
                        then makeAttackTurn (vftovm targetPosition)
                        else NoTurn
                    else NoTurn
            else NoTurn

        static let determineDesiredEnemyTurn indexOpt occupationMap model =
            let characterModel = GameplayModel.getCharacterByIndex indexOpt model
            match characterModel.CharacterState.ControlType with
            | PlayerControlled as controlType ->
                Log.debug ("Invalid ControlType '" + scstring controlType + "' for enemy.")
                NoTurn
            | Chaos ->
                let nextPlayerPosition =
                    match model.Player.CharacterActivityState with
                    | Action _ -> model.Player.Position
                    | Navigation navigationDescriptor -> navigationDescriptor.NextPosition
                    | NoActivity -> model.Player.Position
                if Math.arePositionsAdjacent characterModel.Position nextPlayerPosition then
                    let enemyTurn = makeAttackTurn (vftovm nextPlayerPosition)
                    enemyTurn
                else
                    let randResult = Gen.random1 4
                    let direction = Direction.fromInt randResult
                    let enemyTurn = determineCharacterTurnFromDirection indexOpt direction occupationMap model
                    enemyTurn
            | Uncontrolled -> NoTurn

        static let determineDesiredEnemyTurns occupationMap model =
            let (_, enemyTurns, model) =
                List.foldBack
                    (fun enemy (occupationMap, enemyTurns, model) ->
                        let enemyTurn = determineDesiredEnemyTurn enemy.EnemyIndexOpt occupationMap model
                        let occupationMap = OccupationMap.transferByDesiredTurn enemyTurn enemy.Position occupationMap
                        (occupationMap, enemyTurn :: enemyTurns, model))
                    (model.Enemies)
                    (occupationMap, [], model)
            enemyTurns

        static let determinePlayerTurnFromTouch touchPosition model world =
            let fieldMap = (Simulants.Field.GetFieldModel world).FieldMapNp
            let enemies = getEnemies world
            let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
            if not (anyTurnsInProgress2 Simulants.Player enemies world) then
                let touchPositionW = World.mouseToWorld false touchPosition world
                let occupationMapWithAdjacentEnemies =
                    OccupationMap.makeFromFieldTilesAndAdjacentCharacters
                        (vftovm (Simulants.Player.GetPosition world))
                        fieldMap.FieldTiles
                        enemyPositions
                match determineCharacterTurnFromTouch touchPositionW occupationMapWithAdjacentEnemies Simulants.Player enemies model world with
                | ActionTurn _ as actionTurn -> actionTurn
                | NavigationTurn navigationDescriptor as navigationTurn ->
                    let headNavigationNode = navigationDescriptor.NavigationPathOpt |> Option.get |> List.head
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                    if Map.find headNavigationNode.PositionM occupationMapWithEnemies then CancelTurn
                    else navigationTurn
                | CancelTurn -> CancelTurn
                | NoTurn -> NoTurn
            else NoTurn

        static let determinePlayerTurnFromDetailNavigation direction model world =
            let fieldMap = (Simulants.Field.GetFieldModel world).FieldMapNp
            let enemies = getEnemies world
            let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
            if not (anyTurnsInProgress2 Simulants.Player enemies world) then
                let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                determineCharacterTurnFromDirection None direction occupationMapWithEnemies model
            else NoTurn

        static let determinePlayerTurnFromInput playerInput model world =
            match (Simulants.Player.GetCharacterModel world).CharacterState.ControlType with
            | PlayerControlled ->
                match playerInput with
                | TouchInput touchPosition -> determinePlayerTurnFromTouch touchPosition model world
                | DetailInput direction -> determinePlayerTurnFromDetailNavigation direction model world
                | NoInput -> NoTurn
            | Chaos ->
                Log.debug ("Invalid ControlType 'Chaos' for player.")
                NoTurn
            | Uncontrolled -> NoTurn

        static let determinePlayerTurnFromNavigationProgress model =
            match model.Player.CharacterActivityState with
            | Action _ -> NoTurn
            | Navigation navigationDescriptor ->
                let walkDescriptor = navigationDescriptor.WalkDescriptor
                if model.Player.Position = vmtovf walkDescriptor.WalkOriginM then
                    let fieldMap = Option.get model.FieldMapOpt
                    let enemyPositions = GameplayModel.getEnemyPositions model |> List.toSeq
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                    let walkDestinationM = walkDescriptor.WalkOriginM + dtovm walkDescriptor.WalkDirection
                    if Map.find walkDestinationM occupationMapWithEnemies then CancelTurn
                    else NavigationTurn navigationDescriptor
                else NoTurn
            | NoActivity -> NoTurn

        static let determineEnemyActivities model =
            let enemyTurns = GameplayModel.getEnemyDesiredTurns model
            
            // if any action is present, all activities but the currently active action, if present, or the first action turn in the list, is cancelled
            let currentEnemyActionActivity = List.exists (fun model -> model.CharacterActivityState.IsActing) model.Enemies
            let desiredEnemyActionActivity = List.exists (fun (turn : Turn) -> match turn with | ActionTurn a -> true; | _ -> false) enemyTurns
                                                                               // ^----- better way?
            
            let enemyTurnsFiltered =
                if currentEnemyActionActivity then List.map (fun _ -> NoTurn) enemyTurns
                elif desiredEnemyActionActivity then
                    let firstActionIndex = List.tryFindIndex (fun (turn : Turn) -> match turn with | ActionTurn a -> true; | _ -> false) enemyTurns |> Option.get
                    List.mapi (fun index (turn : Turn) -> if index = firstActionIndex then turn else NoTurn ) enemyTurns
                else enemyTurns

            List.map
                (fun (turn : Turn) ->
                    match turn with
                    | ActionTurn actionDescriptor -> Action actionDescriptor
                    | NavigationTurn navigationDescriptor -> Navigation navigationDescriptor
                    | CancelTurn -> NoActivity
                    | NoTurn -> NoActivity)
                enemyTurnsFiltered
            
        static let setCharacterActivity indexOpt newActivity model =
            match newActivity with
            | Action newActionDescriptor ->
                GameplayModel.updateCharacterActivityState indexOpt (Action newActionDescriptor) model
            | Navigation newNavigationDescriptor ->
                let newNavigationDescriptor = {newNavigationDescriptor with LastWalkOriginM = newNavigationDescriptor.WalkDescriptor.WalkOriginM}
                GameplayModel.updateCharacterActivityState indexOpt (Navigation newNavigationDescriptor) model
            | NoActivity ->
                GameplayModel.updateCharacterActivityState indexOpt NoActivity model

        static let trySetEnemyActivity model newActivity enemy =
            match newActivity with
            | NoActivity -> model
            | _ ->
                let model = GameplayModel.updateDesiredTurnOpt enemy.EnemyIndexOpt (Some NoTurn) model
                setCharacterActivity enemy.EnemyIndexOpt newActivity model
        
        static let setEnemyActivities enemyActivities model =
            Seq.fold2 trySetEnemyActivity model enemyActivities model.Enemies
        
        static let updateCharacterByWalk index walkDescriptor (character : Entity) model world =
            let (newPosition, walkState) = walk walkDescriptor (character.GetPosition world)
            let characterAnimationState = { (character.GetCharacterModel world).CharacterAnimationState with Direction = walkDescriptor.WalkDirection }
            let model = writeCharactersToGameplay model world
            let model = GameplayModel.updateCharacterAnimationState index characterAnimationState model
            let model = GameplayModel.updatePosition index newPosition model
            let world = Simulants.Gameplay.SetGameplayModel model world
            (walkState, world)

        static let updateCharacterByWalkState index walkState navigationDescriptor (character : Entity) model world =
            let model = writeCharactersToGameplay model world
            match walkState with
            | WalkFinished ->
                let lastOrigin = navigationDescriptor.WalkDescriptor.WalkOriginM
                match navigationDescriptor.NavigationPathOpt with
                | Some [] -> failwith "NavigationPath should never be empty here."
                | Some (_ :: []) ->
                    let model = GameplayModel.updateCharacterActivityState index NoActivity model
                    Simulants.Gameplay.SetGameplayModel model world
                | Some (currentNode :: navigationPath) ->
                    let walkDirection = vmtod ((List.head navigationPath).PositionM - currentNode.PositionM)
                    let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = vftovm (character.GetPosition world) }
                    let navigationDescriptor = { WalkDescriptor = walkDescriptor; NavigationPathOpt = Some navigationPath; LastWalkOriginM = lastOrigin }
                    let model = GameplayModel.updateCharacterActivityState index (Navigation navigationDescriptor) model
                    Simulants.Gameplay.SetGameplayModel model world
                | None ->
                    let model = GameplayModel.updateCharacterActivityState index NoActivity model
                    Simulants.Gameplay.SetGameplayModel model world
            | WalkContinuing -> world

        static let tickNavigation navigationDescriptor (character : Entity) model world =
            let index = (character.GetCharacterModel world).EnemyIndexOpt
            let (walkState, world) = updateCharacterByWalk index navigationDescriptor.WalkDescriptor character model world
            updateCharacterByWalkState index walkState navigationDescriptor character model world

        static let tickReaction initiatorIndexOpt actionDescriptor model =
            let initiatorModel = GameplayModel.getCharacterByIndex initiatorIndexOpt model
            let reactorModel = GameplayModel.getCharacterInDirection initiatorIndexOpt initiatorModel.CharacterAnimationState.Direction model
            let reactorState = reactorModel.CharacterState
            let reactorIndexOpt = reactorModel.EnemyIndexOpt
            if actionDescriptor.ActionTicks = (Constants.InfinityRpg.CharacterAnimationActingDelay * 2L) then
                let reactorDamage = 4 // NOTE: just hard-coding damage for now
                let model = GameplayModel.updateCharacterState reactorIndexOpt { reactorState with HitPoints = reactorState.HitPoints - reactorDamage } model
                let reactorModel = GameplayModel.getCharacterByIndex reactorIndexOpt model
                if reactorModel.CharacterState.HitPoints <= 0 then
                    let characterAnimationState = reactorModel.CharacterAnimationState
                    GameplayModel.updateCharacterAnimationState reactorIndexOpt { characterAnimationState with AnimationType = CharacterAnimationSlain } model
                else model
            elif actionDescriptor.ActionTicks = Constants.InfinityRpg.ActionTicksMax then
                if reactorModel.CharacterState.HitPoints <= 0 then
                    match reactorIndexOpt with
                    | None -> GameplayModel.updateCharacterState None {reactorState with ControlType = Uncontrolled} model // TODO: reimplement screen transition
                    | Some index -> GameplayModel.removeEnemy index model
                else model
            else model

        static let tickAction indexOpt actionDescriptor model world =
            let model = writeCharactersToGameplay model world
            let characterModel = GameplayModel.getCharacterByIndex indexOpt model
            let characterAnimationState = characterModel.CharacterAnimationState
            let actionInc = Action { actionDescriptor with ActionTicks = inc actionDescriptor.ActionTicks }
            if actionDescriptor.ActionTicks = 0L then
                let newAnimation = getCharacterAnimationStateByActionBegin (World.getTickTime world) characterModel.Position characterAnimationState actionDescriptor
                let model = GameplayModel.updateCharacterAnimationState indexOpt newAnimation model
                GameplayModel.updateCharacterActivityState indexOpt actionInc model
            elif actionDescriptor.ActionTicks < (Constants.InfinityRpg.CharacterAnimationActingDelay * 2L) then
                GameplayModel.updateCharacterActivityState indexOpt actionInc model
            elif actionDescriptor.ActionTicks < Constants.InfinityRpg.ActionTicksMax then
                let model = GameplayModel.updateCharacterActivityState indexOpt actionInc model
                tickReaction indexOpt actionDescriptor model
            else
                let newAnimation = getCharacterAnimationStateByActionEnd (World.getTickTime world) characterAnimationState
                let model = GameplayModel.updateCharacterAnimationState indexOpt newAnimation model
                let model = GameplayModel.updateCharacterActivityState indexOpt NoActivity model
                tickReaction indexOpt actionDescriptor model
        
        static let tickUpdate model world =
            
            (* set enemy activities in accordance with the player's current activity.
            "NoActivity" here means the player's turn is finished, and it's the enemy's turn.
            the present location of this code reflects the fact that the turn in tickNewTurn means the entire round.
            dividing this initialization into turns for individual characters will arguably make the code more intuitive. *)
            
            let model =
                match model.Player.CharacterActivityState with
                | Action _ -> model
                | Navigation _ 
                | NoActivity ->
                    let newEnemyActivities = determineEnemyActivities model
                    if List.exists (fun (state : CharacterActivityState) -> state.IsActing) newEnemyActivities then
                        let model = setEnemyActivities newEnemyActivities model
                        cancelNavigation None model
                    else setEnemyActivities newEnemyActivities model
            
            let world = Simulants.Gameplay.SetGameplayModel model world
            
            let enemies = getEnemies world |> Seq.toList
            let characters = Simulants.Player :: enemies
            let rec recursion (characters : Entity list) model world =
                if characters.Length = 0 then (model,world)
                else
                    let character = characters.Head
                    let indexOpt = (character.GetCharacterModel world).EnemyIndexOpt
                    let characterModel = GameplayModel.getCharacterByIndex indexOpt model
                    let world =
                        match characterModel.CharacterActivityState with
                        | Action actionDescriptor ->
                            let model = tickAction indexOpt actionDescriptor model world
                            Simulants.Gameplay.SetGameplayModel model world
                        | Navigation navigationDescriptor ->
                            if navigationDescriptor.LastWalkOriginM = navigationDescriptor.WalkDescriptor.WalkOriginM then
                                tickNavigation navigationDescriptor character model world
                            else world
                        | NoActivity -> world
                    recursion characters.Tail model world
            
            let (model, world) = recursion characters model world
            just world
        
        static let tickNewTurn newPlayerTurn model world =

            let newPlayerActivity =
                match newPlayerTurn with
                | ActionTurn actionDescriptor -> Action actionDescriptor
                | NavigationTurn navigationDescriptor -> Navigation navigationDescriptor
                | CancelTurn -> NoActivity
                | NoTurn -> failwith "newPlayerTurn cannot be NoTurn at this point."
            
            // here None means player
            let model = setCharacterActivity None newPlayerActivity model

            // determine (and set) enemy desired turns if applicable
            let occupationMap =
                let fieldMap = Option.get model.FieldMapOpt
                let enemyPositions = GameplayModel.getEnemyPositions model |> List.toSeq
                OccupationMap.makeFromFieldTilesAndCharactersAndDesiredTurn fieldMap.FieldTiles enemyPositions newPlayerTurn
            
            let model =
                match newPlayerActivity with
                | Action _
                | Navigation _ ->
                    let enemyDesiredTurns = determineDesiredEnemyTurns occupationMap model
                    let model =
                        Seq.fold2
                            (fun model enemy turn -> GameplayModel.updateDesiredTurnOpt enemy.EnemyIndexOpt (Some turn) model)
                            model
                            model.Enemies
                            enemyDesiredTurns
                    model
                | NoActivity -> model

            let world = Simulants.Gameplay.SetGameplayModel model world

            tickUpdate model world
        
        static let tickTurn model world =

            let playerTurn = determinePlayerTurnFromNavigationProgress model

            match playerTurn with
            | NoTurn -> tickUpdate model world
            | _ -> tickNewTurn playerTurn model world

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
             Simulants.Title.SelectEvent => cmd QuittingGameplay
             Simulants.Gameplay.DeselectEvent => cmd QuitGameplay]

        override this.Message (model, message, _, _) =
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

                let player =
                    { Position = Vector2.Zero
                      EnemyIndexOpt = None
                      CharacterActivityState = NoActivity
                      CharacterState = { CharacterState.empty with HitPoints = 30; ControlType = PlayerControlled }
                      CharacterAnimationState = { StartTime = 0L; AnimationType = CharacterAnimationFacing; Direction = Upward }
                      CharacterAnimationSheet = Assets.PlayerImage
                      DesiredTurnOpt = None }
                
                let model = { model with FieldMapOpt = Some fieldMap; Enemies = enemies; Player = player }
                just model
        
        override this.Command (model, command, screen, world) =
            match command with
            | ToggleHaltButton ->
                let world = Simulants.HudHalt.SetEnabled (isPlayerNavigatingPath world) world
                just world
            | HandlePlayerInput input ->
                if (anyTurnsInProgress world) then just world
                else
                    let world = Simulants.HudSaveGame.SetEnabled false world
                    let playerTurn = determinePlayerTurnFromInput input model world
                    match playerTurn with
                    | NoTurn -> just world
                    | _ -> tickNewTurn playerTurn model world
            | SaveGame gameplay ->
                World.writeScreenToFile Assets.SaveFilePath gameplay world
                just world
            | QuittingGameplay ->
            //    let world = World.playSong Constants.Audio.DefaultFadeOutMs 1.0f Assets.ButterflyGirlSong world
                just world
            | QuitGameplay ->
                let world = World.destroyLayer Simulants.Scene world
                just world
            | RunGameplay ->
            //    let world = World.playSong Constants.Audio.DefaultFadeOutMs 1.0f Assets.HerosVengeanceSong world
                withMsg world NewGame
            | Tick ->
                if (anyTurnsInProgress world) then tickTurn model world
                elif KeyboardState.isKeyDown KeyboardKey.Up then withCmd world (HandlePlayerInput (DetailInput Upward))
                elif KeyboardState.isKeyDown KeyboardKey.Right then withCmd world (HandlePlayerInput (DetailInput Rightward))
                elif KeyboardState.isKeyDown KeyboardKey.Down then withCmd world (HandlePlayerInput (DetailInput Downward))
                elif KeyboardState.isKeyDown KeyboardKey.Left then withCmd world (HandlePlayerInput (DetailInput Leftward))
                elif not (Simulants.HudSaveGame.GetEnabled world) then just (Simulants.HudSaveGame.SetEnabled true world)
                else just world
            | Nop -> just world

        override this.Content (model, screen) =

            [Content.layer Simulants.Scene.Name []

                [Content.entityOpt model (fun model -> model.FieldMapOpt) (fun _ fieldMap world ->
                    let fieldMap = fieldMap.Get world
                    Content.entity<FieldDispatcher> Simulants.Field.Name
                        [Entity.FieldModel == { FieldMapNp = fieldMap }
                         Entity.Size == vmtovf fieldMap.FieldSizeM
                         Entity.Persistent == false])

                 Content.entitiesIndexedBy model
                     (fun model -> model.Enemies) constant
                     (fun model -> Option.get model.EnemyIndexOpt)
                     (fun index model _ ->
                        Content.entity<EnemyDispatcher> ("Enemy+" + scstring index)
                            [Entity.CharacterModel <== model])

                 Content.entity<PlayerDispatcher> Simulants.Player.Name // TODO: didn't realise enemies' possible placements included outermost tiles allowing player/enemy overlap. another problem to deal with once structure is under control
                    [Entity.CharacterModel <== model --> fun model ->
                        model.Player]]]
