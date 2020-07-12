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

        static let getCharacters world =
            let entities = World.getEntities Simulants.Scene world
            Seq.filter (fun (entity : Entity) -> entity.Is<CharacterDispatcher> world) entities

        static let tryGetCharacterAtPosition position world =
            let characters = getCharacters world
            Seq.tryFind (fun (character : Entity) -> character.GetPosition world = position) characters
        
        static let tryGetCharacterInDirection position direction world =
            tryGetCharacterAtPosition (position + dtovf direction) world
        
        static let getCharacterInDirection position direction world =
            Option.get (tryGetCharacterInDirection position direction world)

        static let getEnemies world =
            let entities = World.getEntities Simulants.Scene world
            Seq.filter (fun (entity : Entity) -> entity.Is<EnemyDispatcher> world) entities

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
            let enemyCount = randResult + 1
            let (models, _) =
                List.fold
                    (fun (models, coords) index ->
                        let availableCoordinates = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles coords |> Map.filter (fun _ occupied -> occupied = false) |> Map.toKeyList |> List.toArray
                        let randResult = Gen.random1 availableCoordinates.Length
                        let enemyCoordinates = vmtovf availableCoordinates.[randResult]
                        let model =
                            { InitialPosition = enemyCoordinates
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

        static let cancelNavigation (character : Entity) world =
            let characterActivity =
                match (character.GetCharacterModel world).CharacterActivityState with
                | Action _ as action -> action
                | NoActivity -> NoActivity
                | Navigation navDescriptor -> Navigation { navDescriptor with NavigationPathOpt = None }
            character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = characterActivity } world

        static let anyTurnsInProgress2 (player : Entity) enemies world =
            (player.GetCharacterModel world).CharacterActivityState <> NoActivity ||
            Seq.exists
                (fun (enemy : Entity) -> (enemy.GetCharacterModel world).DesiredTurnOpt <> Some NoTurn || (enemy.GetCharacterModel world).CharacterActivityState <> NoActivity)
                enemies

        static let anyTurnsInProgress world =
            let enemies = getEnemies world
            anyTurnsInProgress2 Simulants.Player enemies world

        static let determineCharacterTurnFromDirection direction occupationMap (character : Entity) opponents world =
            match (character.GetCharacterModel world).CharacterActivityState with
            | Action _ -> NoTurn
            | Navigation _ -> NoTurn
            | NoActivity ->
                let openDirections = OccupationMap.getOpenDirectionsAtPositionM (vftovm (character.GetPosition world)) occupationMap
                if Set.contains direction openDirections then
                    let characterPositionM = vftovm (character.GetPosition world)
                    let walkDescriptor = { WalkDirection = direction; WalkOriginM = characterPositionM }
                    NavigationTurn { WalkDescriptor = walkDescriptor; NavigationPathOpt = None; LastWalkOriginM = characterPositionM }
                else
                    let targetPosition = character.GetPosition world + dtovf direction
                    if Seq.exists (fun (opponent : Entity) -> opponent.GetPosition world = targetPosition) opponents
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

        static let determineDesiredEnemyTurn occupationMap (player : Entity) (enemy : Entity) world =
            match (enemy.GetCharacterModel world).CharacterState.ControlType with
            | PlayerControlled as controlType ->
                Log.debug ("Invalid ControlType '" + scstring controlType + "' for enemy.")
                NoTurn
            | Chaos ->
                let nextPlayerPosition =
                    match (player.GetCharacterModel world).CharacterActivityState with
                    | Action _ -> player.GetPosition world
                    | Navigation navigationDescriptor -> navigationDescriptor.NextPosition
                    | NoActivity -> player.GetPosition world
                if Math.arePositionsAdjacent (enemy.GetPosition world) nextPlayerPosition then
                    let enemyTurn = makeAttackTurn (vftovm nextPlayerPosition)
                    enemyTurn
                else
                    let randResult = Gen.random1 4
                    let direction = Direction.fromInt randResult
                    let enemyTurn = determineCharacterTurnFromDirection direction occupationMap enemy [player] world
                    enemyTurn
            | Uncontrolled -> NoTurn

        static let determineDesiredEnemyTurns occupationMap player enemies model world =
            let (_, enemyTurns) =
                List.foldBack
                    (fun (enemy : Entity) (occupationMap, enemyTurns) ->
                        let enemyTurn = determineDesiredEnemyTurn occupationMap player enemy world
                        let occupationMap = OccupationMap.transferByDesiredTurn enemyTurn (enemy.GetPosition world) occupationMap
                        (occupationMap, enemyTurn :: enemyTurns))
                    (List.ofSeq enemies)
                    (occupationMap, [])
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
                determineCharacterTurnFromDirection direction occupationMapWithEnemies Simulants.Player enemies world
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

        static let determinePlayerTurnFromNavigationProgress model world =
            match (Simulants.Player.GetCharacterModel world).CharacterActivityState with
            | Action _ -> NoTurn
            | Navigation navigationDescriptor ->
                let walkDescriptor = navigationDescriptor.WalkDescriptor
                if Simulants.Player.GetPosition world = vmtovf walkDescriptor.WalkOriginM then
                    let fieldMap = (Simulants.Field.GetFieldModel world).FieldMapNp
                    let enemies = getEnemies world
                    let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                    let walkDestinationM = walkDescriptor.WalkOriginM + dtovm walkDescriptor.WalkDirection
                    if Map.find walkDestinationM occupationMapWithEnemies then CancelTurn
                    else NavigationTurn navigationDescriptor
                else NoTurn
            | NoActivity -> NoTurn

        static let determineEnemyActivities enemies model world =
            let enemyTurns = List.ofSeq enemies |> List.map (fun (enemy : Entity) -> Option.get (enemy.GetCharacterModel world).DesiredTurnOpt)
            
            // if any action is present, all activities but the currently active action, if present, or the first action turn in the list, is cancelled
            let currentEnemyActionActivity = Seq.exists (fun (enemy : Entity) -> (enemy.GetCharacterModel world).CharacterActivityState.IsActing) enemies
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
            
        static let setCharacterActivity newActivity (character : Entity) world =
            match newActivity with
            | Action newActionDescriptor -> character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = (Action newActionDescriptor) } world
            | Navigation newNavigationDescriptor ->
                let newNavigationDescriptor = {newNavigationDescriptor with LastWalkOriginM = newNavigationDescriptor.WalkDescriptor.WalkOriginM}
                character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = (Navigation newNavigationDescriptor) } world
            | NoActivity -> character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = NoActivity } world

        static let trySetEnemyActivity world newActivity (enemy : Entity) =
            match newActivity with
            | NoActivity -> world
            | _ ->
                let world = enemy.SetCharacterModel { enemy.GetCharacterModel world with DesiredTurnOpt = Some NoTurn } world
                setCharacterActivity newActivity enemy world            
        
        static let setEnemyActivities enemyActivities enemies model world =
            Seq.fold2 trySetEnemyActivity world enemyActivities enemies
        
        static let updateCharacterByWalk walkDescriptor (character : Entity) model world =
            let (newPosition, walkState) = walk walkDescriptor (character.GetPosition world)
            let world = character.SetPosition newPosition world
            let characterAnimationState = { (character.GetCharacterModel world).CharacterAnimationState with Direction = walkDescriptor.WalkDirection }
            let world = character.SetCharacterModel { (character.GetCharacterModel world) with CharacterAnimationState = characterAnimationState } world
            (walkState, world)

        static let updateCharacterByWalkState walkState navigationDescriptor (character : Entity) model world =
            match walkState with
            | WalkFinished ->
                let lastOrigin = navigationDescriptor.WalkDescriptor.WalkOriginM
                match navigationDescriptor.NavigationPathOpt with
                | Some [] -> failwith "NavigationPath should never be empty here."
                | Some (_ :: []) -> character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = NoActivity } world
                | Some (currentNode :: navigationPath) ->
                    let walkDirection = vmtod ((List.head navigationPath).PositionM - currentNode.PositionM)
                    let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = vftovm (character.GetPosition world) }
                    let navigationDescriptor = { WalkDescriptor = walkDescriptor; NavigationPathOpt = Some navigationPath; LastWalkOriginM = lastOrigin }
                    character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = (Navigation navigationDescriptor) } world
                | None -> character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = NoActivity } world
            | WalkContinuing -> world

        static let tickNavigation navigationDescriptor character model world =
            let (walkState, world) = updateCharacterByWalk navigationDescriptor.WalkDescriptor character model world
            updateCharacterByWalkState walkState navigationDescriptor character model world

        static let tickReaction actionDescriptor (initiator : Entity) model world =
            let reactor =
                getCharacterInDirection
                    (initiator.GetPosition world)
                    (initiator.GetCharacterModel world).CharacterAnimationState.Direction
                    world
            if actionDescriptor.ActionTicks = (Constants.InfinityRpg.CharacterAnimationActingDelay * 2L) then
                let reactorDamage = 4 // NOTE: just hard-coding damage for now
                let reactorModel = reactor.GetCharacterModel world
                let reactorState = reactorModel.CharacterState
                let world = reactor.SetCharacterModel { reactorModel with CharacterState = { reactorState with HitPoints = reactorState.HitPoints - reactorDamage } } world
                if (reactor.GetCharacterModel world).CharacterState.HitPoints <= 0 then
                    let characterAnimationState = (reactor.GetCharacterModel world).CharacterAnimationState
                    reactor.SetCharacterModel { (reactor.GetCharacterModel world) with CharacterAnimationState = { characterAnimationState with AnimationType = CharacterAnimationSlain } } world
                else world
            elif actionDescriptor.ActionTicks = Constants.InfinityRpg.ActionTicksMax then
                if (reactor.GetCharacterModel world).CharacterState.HitPoints <= 0 then
                    if reactor.Name = Simulants.Player.Name then World.transitionScreen Simulants.Title world else World.destroyEntity reactor world
                else world
            else world

        static let tickAction actionDescriptor (character : Entity) model world =
            let characterAnimationState = (character.GetCharacterModel world).CharacterAnimationState
            if actionDescriptor.ActionTicks = 0L then
                let world = character.SetCharacterModel { (character.GetCharacterModel world) with CharacterAnimationState = (getCharacterAnimationStateByActionBegin (World.getTickTime world) (character.GetPosition world) characterAnimationState actionDescriptor) } world
                character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = (Action { actionDescriptor with ActionTicks = inc actionDescriptor.ActionTicks }) } world
            elif actionDescriptor.ActionTicks < (Constants.InfinityRpg.CharacterAnimationActingDelay * 2L) then
                world |>
                    character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = (Action { actionDescriptor with ActionTicks = inc actionDescriptor.ActionTicks }) }
            elif actionDescriptor.ActionTicks < Constants.InfinityRpg.ActionTicksMax then
                world |>
                    tickReaction actionDescriptor character model |>
                    character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = (Action { actionDescriptor with ActionTicks = inc actionDescriptor.ActionTicks }) }
            else
                let world = character.SetCharacterModel { character.GetCharacterModel world with CharacterActivityState = NoActivity } world
                world |>                    
                    character.SetCharacterModel { character.GetCharacterModel world with CharacterAnimationState = (getCharacterAnimationStateByActionEnd (World.getTickTime world) characterAnimationState ) } |>
                    tickReaction actionDescriptor character model
        
        static let tickUpdate model world =
            // set enemy activities in accordance with the player's current activity
            let world =
                let enemies = getEnemies world
                match (Simulants.Player.GetCharacterModel world).CharacterActivityState with
                | Action _ -> world
                | Navigation _ 
                | NoActivity ->
                    let newEnemyActivities = determineEnemyActivities enemies model world
                    if List.exists (fun (state : CharacterActivityState) -> state.IsActing) newEnemyActivities then
                        let world = setEnemyActivities newEnemyActivities enemies model world
                        cancelNavigation Simulants.Player world
                    else setEnemyActivities newEnemyActivities enemies model world
            
            let enemies = getEnemies world |> Seq.toList
            let characters = Simulants.Player :: enemies
            let rec recursion (characters : Entity list) world =
                if characters.Length = 0 then world
                else
                    let character = characters.Head
                    let world =
                        match (character.GetCharacterModel world).CharacterActivityState with
                        | Action actionDescriptor ->
                            tickAction actionDescriptor character model world
                        | Navigation navigationDescriptor ->
                            if navigationDescriptor.LastWalkOriginM = navigationDescriptor.WalkDescriptor.WalkOriginM then
                                tickNavigation navigationDescriptor character model world
                            else world
                        | NoActivity -> world
                    recursion characters.Tail world
            just (recursion characters world)
        
        static let tickNewTurn newPlayerTurn model world =

            let newPlayerActivity =
                match newPlayerTurn with
                | ActionTurn actionDescriptor -> Action actionDescriptor
                | NavigationTurn navigationDescriptor -> Navigation navigationDescriptor
                | CancelTurn -> NoActivity
                | NoTurn -> failwith "newPlayerTurn cannot be NoTurn at this point."
            
            let world = setCharacterActivity newPlayerActivity Simulants.Player world

            // determine (and set) enemy desired turns if applicable
            let occupationMap =
                let fieldMap = (Simulants.Field.GetFieldModel world).FieldMapNp
                let enemies = getEnemies world
                let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
                OccupationMap.makeFromFieldTilesAndCharactersAndDesiredTurn fieldMap.FieldTiles enemyPositions newPlayerTurn
            
            let world =
                match newPlayerActivity with
                | Action _
                | Navigation _ ->
                    let enemies = getEnemies world
                    let enemyDesiredTurns = determineDesiredEnemyTurns occupationMap Simulants.Player enemies model world
                    Seq.fold2 (fun world (enemy : Entity) turn -> enemy.SetCharacterModel { enemy.GetCharacterModel world with DesiredTurnOpt = Some turn } world) world enemies enemyDesiredTurns
                | NoActivity -> world

            tickUpdate model world
        
        static let tickTurn model world =

            let playerTurn = determinePlayerTurnFromNavigationProgress model world

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
                    { InitialPosition = Vector2.Zero
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
                let world = World.playSong Constants.Audio.DefaultFadeOutMs 1.0f Assets.ButterflyGirlSong world
                just world
            | QuitGameplay ->
                let world = World.destroyLayer Simulants.Scene world
                just world
            | RunGameplay ->
                let world = World.playSong Constants.Audio.DefaultFadeOutMs 1.0f Assets.HerosVengeanceSong world
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
                     (fun index model world ->
                        let initialPosition = (model.Get world).InitialPosition
                        Content.entity<EnemyDispatcher> ("Enemy+" + scstring index)
                            [Entity.Position == initialPosition
                             Entity.CharacterModel <== model])

                 Content.entity<PlayerDispatcher> Simulants.Player.Name // TODO: didn't realise enemies' possible placements included outermost tiles allowing player/enemy overlap. another problem to deal with once structure is under control
                    [Entity.CharacterModel <== model --> fun model ->
                        model.Player]]]
