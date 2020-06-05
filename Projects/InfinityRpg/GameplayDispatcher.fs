namespace InfinityRpg
open System
open System.IO
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module GameplayDispatcherModule =

    type [<StructuralEquality; NoComparison>] PlayerInput =
        | TouchInput of Vector2
        | DetailInput of Direction
        | NoInput

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
        
        member this.GetContentRandState = this.Get Property? ContentRandState
        member this.SetContentRandState = this.Set Property? ContentRandState
        member this.ContentRandState = lens<uint64> Property? ContentRandState this.GetContentRandState this.SetContentRandState this
        member this.GetShallLoadGame = this.Get Property? ShallLoadGame
        member this.SetShallLoadGame = this.Set Property? ShallLoadGame
        member this.ShallLoadGame = lens<bool> Property? ShallLoadGame this.GetShallLoadGame this.SetShallLoadGame this

    type GameplayDispatcher () =
        inherit ScreenDispatcher<unit, unit, GameplayCommand> (())

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

        static let createField scene rand world =
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
            let (field, world) = World.createEntity<FieldDispatcher> (Some Simulants.Field.Name) DefaultOverlay scene world
            let world = field.SetFieldMapNp fieldMap world
            let world = field.SetSize (field.GetQuickSize world) world
            let world = field.SetPersistent false world
            (field, rand, world)

        static let createEnemies scene rand world =
            let (randResult, rand) = Rand.nextIntUnder 5 rand
            let enemyCount = randResult + 1
            List.fold
                (fun (enemies, rand, world) _ ->
                    let fieldMap = Simulants.Field.GetFieldMapNp world
                    let characters = getCharacters world
                    let characterPositions = Seq.map (fun (character : Entity) -> character.GetPosition world) characters
                    let availableCoordinates = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles characterPositions |> Map.filter (fun _ occupied -> occupied = false) |> Map.toKeyList |> List.toArray
                    let (randResult, rand) = Rand.nextIntUnder availableCoordinates.Length rand
                    let enemyCoordinates = availableCoordinates.[randResult]
                    let (enemy, world) = World.createEntity<EnemyDispatcher> None DefaultOverlay scene world
                    let world = enemy.SetPosition (vmtovf enemyCoordinates) world
                    let world = enemy.SetDepth Constants.Layout.CharacterDepth world
                    let world = enemy.SetCharacterAnimationSheet Assets.GoopyImage world
                    (enemy :: enemies, rand, world))
                ([], rand, world)
                [0 .. enemyCount - 1]

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
            (Simulants.Player.GetCharacterActivityState world).IsNavigatingPath

        static let cancelNavigation (character : Entity) world =
            let characterActivity =
                match character.GetCharacterActivityState world with
                | Action _ as action -> action
                | NoActivity -> NoActivity
                | Navigation navDescriptor -> Navigation { navDescriptor with NavigationPathOpt = None }
            character.SetCharacterActivityState characterActivity world

        static let anyTurnsInProgress2 (player : Entity) enemies world =
            player.GetCharacterActivityState world <> NoActivity ||
            Seq.exists
                (fun (enemy : Entity) -> enemy.GetDesiredTurn world <> NoTurn || enemy.GetCharacterActivityState world <> NoActivity)
                enemies

        static let anyTurnsInProgress world =
            let enemies = getEnemies world
            anyTurnsInProgress2 Simulants.Player enemies world

        

        static let determineCharacterTurnFromDirection direction occupationMap (character : Entity) opponents world =
            match character.GetCharacterActivityState world with
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

        static let determineCharacterTurnFromTouch touchPosition occupationMap (character : Entity) opponents world =
            if character.GetCharacterActivityState world = NoActivity then
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
            match (enemy.GetCharacterState world).ControlType with
            | PlayerControlled as controlType ->
                Log.debug ("Invalid ControlType '" + scstring controlType + "' for enemy.")
                NoTurn
            | Chaos ->
                let nextPlayerPosition =
                    match player.GetCharacterActivityState world with
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

        static let determineDesiredEnemyTurns occupationMap player enemies world =
            let (_, enemyTurns) =
                List.foldBack
                    (fun (enemy : Entity) (occupationMap, enemyTurns) ->
                        let enemyTurn = determineDesiredEnemyTurn occupationMap player enemy world
                        let occupationMap = OccupationMap.transferByDesiredTurn enemyTurn (enemy.GetPosition world) occupationMap
                        (occupationMap, enemyTurn :: enemyTurns))
                    (List.ofSeq enemies)
                    (occupationMap, [])
            enemyTurns

        static let determinePlayerTurnFromTouch touchPosition world =
            let fieldMap = Simulants.Field.GetFieldMapNp world
            let enemies = getEnemies world
            let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
            if not (anyTurnsInProgress2 Simulants.Player enemies world) then
                let touchPositionW = World.mouseToWorld Relative touchPosition world
                let occupationMapWithAdjacentEnemies =
                    OccupationMap.makeFromFieldTilesAndAdjacentCharacters
                        (vftovm (Simulants.Player.GetPosition world))
                        fieldMap.FieldTiles
                        enemyPositions
                match determineCharacterTurnFromTouch touchPositionW occupationMapWithAdjacentEnemies Simulants.Player enemies world with
                | ActionTurn _ as actionTurn -> actionTurn
                | NavigationTurn navigationDescriptor as navigationTurn ->
                    let headNavigationNode = navigationDescriptor.NavigationPathOpt |> Option.get |> List.head
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                    if Map.find headNavigationNode.PositionM occupationMapWithEnemies then CancelTurn
                    else navigationTurn
                | CancelTurn -> CancelTurn
                | NoTurn -> NoTurn
            else NoTurn

        static let determinePlayerTurnFromDetailNavigation direction world =
            let fieldMap = Simulants.Field.GetFieldMapNp world
            let enemies = getEnemies world
            let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
            if not (anyTurnsInProgress2 Simulants.Player enemies world) then
                let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                determineCharacterTurnFromDirection direction occupationMapWithEnemies Simulants.Player enemies world
            else NoTurn

        static let determinePlayerTurnFromInput playerInput world =
            match (Simulants.Player.GetCharacterState world).ControlType with
            | PlayerControlled ->
                match playerInput with
                | TouchInput touchPosition -> determinePlayerTurnFromTouch touchPosition world
                | DetailInput direction -> determinePlayerTurnFromDetailNavigation direction world
                | NoInput -> NoTurn
            | Chaos ->
                Log.debug ("Invalid ControlType 'Chaos' for player.")
                NoTurn
            | Uncontrolled -> NoTurn

        static let determinePlayerTurnFromNavigationProgress world =
            match Simulants.Player.GetCharacterActivityState world with
            | Action _ -> NoTurn
            | Navigation navigationDescriptor ->
                let walkDescriptor = navigationDescriptor.WalkDescriptor
                if Simulants.Player.GetPosition world = vmtovf walkDescriptor.WalkOriginM then
                    let fieldMap = Simulants.Field.GetFieldMapNp world
                    let enemies = getEnemies world
                    let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                    let walkDestinationM = walkDescriptor.WalkOriginM + dtovm walkDescriptor.WalkDirection
                    if Map.find walkDestinationM occupationMapWithEnemies then CancelTurn
                    else NavigationTurn navigationDescriptor
                else NoTurn
            | NoActivity -> NoTurn

        static let determineEnemyActivities enemies world =
            List.foldBack
                (fun (enemy : Entity) precedingEnemyActivities ->
                    let enemyActivity =
                        let noPrecedingEnemyActionActivity = Seq.notExists (fun (state : CharacterActivityState) -> state.IsActing) precedingEnemyActivities
                        let noCurrentEnemyActionActivity = Seq.notExists (fun (enemy : Entity) -> (enemy.GetCharacterActivityState world).IsActing) enemies
                        if noPrecedingEnemyActionActivity && noCurrentEnemyActionActivity then
                            match enemy.GetDesiredTurn world with
                            | ActionTurn actionDescriptor -> Action actionDescriptor
                            | NavigationTurn navigationDescriptor -> Navigation navigationDescriptor
                            | CancelTurn -> NoActivity
                            | NoTurn -> NoActivity
                        else NoActivity
                    enemyActivity :: precedingEnemyActivities)
                (List.ofSeq enemies)
                []

        static let setCharacterActivity newActivity (character : Entity) world =
            match newActivity with
            | Action newActionDescriptor -> character.SetCharacterActivityState (Action newActionDescriptor) world
            | Navigation newNavigationDescriptor ->
                let newNavigationDescriptor = {newNavigationDescriptor with LastWalkOriginM = newNavigationDescriptor.WalkDescriptor.WalkOriginM}
                character.SetCharacterActivityState (Navigation newNavigationDescriptor) world
            | NoActivity -> character.SetCharacterActivityState NoActivity world

        static let trySetEnemyNavigation world newActivity (enemy : Entity) =
            match newActivity with
            | Navigation _ ->
                let world = enemy.SetDesiredTurn NoTurn world
                setCharacterActivity newActivity enemy world
            | _ -> world

        static let trySetEnemyAction world newActivity (enemy : Entity) =
            match newActivity with
            | Action _ ->
                let world = enemy.SetDesiredTurn NoTurn world
                setCharacterActivity newActivity enemy world
            | _ -> world
        
        static let setEnemyActivities enemyActivities enemies world =
            let anyEnemyActionActivity = Seq.exists (fun (state : CharacterActivityState) -> state.IsActing) enemyActivities
            if anyEnemyActionActivity then
                (Seq.fold2 trySetEnemyAction world enemyActivities enemies)
            else (Seq.fold2 trySetEnemyNavigation world enemyActivities enemies)

        
        static let updateCharacterByWalk walkDescriptor (character : Entity) world =
            let (newPosition, walkState) = walk walkDescriptor (character.GetPosition world)
            let world = character.SetPosition newPosition world
            let characterAnimationState = { character.GetCharacterAnimationState world with Direction = walkDescriptor.WalkDirection }
            let world = character.SetCharacterAnimationState characterAnimationState world
            (walkState, world)

        static let updateCharacterByWalkState walkState navigationDescriptor (character : Entity) world =
            match walkState with
            | WalkFinished ->
                let lastOrigin = navigationDescriptor.WalkDescriptor.WalkOriginM
                match navigationDescriptor.NavigationPathOpt with
                | Some [] -> failwith "NavigationPath should never be empty here."
                | Some (_ :: []) -> character.SetCharacterActivityState NoActivity world
                | Some (currentNode :: navigationPath) ->
                    let walkDirection = vmtod ((List.head navigationPath).PositionM - currentNode.PositionM)
                    let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = vftovm (character.GetPosition world) }
                    let navigationDescriptor = { WalkDescriptor = walkDescriptor; NavigationPathOpt = Some navigationPath; LastWalkOriginM = lastOrigin }
                    character.SetCharacterActivityState (Navigation navigationDescriptor) world
                | None -> character.SetCharacterActivityState NoActivity world
            | WalkContinuing -> world

        static let tickNavigation navigationDescriptor character world =
            let (walkState, world) = updateCharacterByWalk navigationDescriptor.WalkDescriptor character world
            updateCharacterByWalkState walkState navigationDescriptor character world

        static let tickReaction actionDescriptor (initiator : Entity) world =
            let reactor =
                getCharacterInDirection
                    (initiator.GetPosition world)
                    (initiator.GetCharacterAnimationState world).Direction
                    world
            if actionDescriptor.ActionTicks = (Constants.InfinityRpg.CharacterAnimationActingDelay * 2L) then
                let reactorDamage = 4 // NOTE: just hard-coding damage for now
                let world = reactor.CharacterState.Update (fun state -> { state with HitPoints = state.HitPoints - reactorDamage }) world
                if reactor.CharacterState.GetBy (fun state -> state.HitPoints <= 0) world then
                    reactor.SetCharacterAnimationState {reactor.GetCharacterAnimationState world with AnimationType = CharacterAnimationSlain} world
                else world
            elif actionDescriptor.ActionTicks = Constants.InfinityRpg.ActionTicksMax then
                if reactor.CharacterState.GetBy (fun state -> state.HitPoints <= 0) world then
                    if reactor.Name = Simulants.Player.Name then World.transitionScreen Simulants.Title world else World.destroyEntity reactor world
                else world
            else world

        static let tickAction actionDescriptor (character : Entity) world =
            if actionDescriptor.ActionTicks = 0L then
                world |>
                    character.SetCharacterAnimationState (getCharacterAnimationStateByActionBegin (World.getTickTime world) (character.GetPosition world) (character.GetCharacterAnimationState world) actionDescriptor) |>
                    character.SetCharacterActivityState (Action { actionDescriptor with ActionTicks = inc actionDescriptor.ActionTicks })
            elif actionDescriptor.ActionTicks < (Constants.InfinityRpg.CharacterAnimationActingDelay * 2L) then
                world |>
                    character.SetCharacterActivityState (Action { actionDescriptor with ActionTicks = inc actionDescriptor.ActionTicks })
            elif actionDescriptor.ActionTicks < Constants.InfinityRpg.ActionTicksMax then
                world |>
                    tickReaction actionDescriptor character |>
                    character.SetCharacterActivityState (Action { actionDescriptor with ActionTicks = inc actionDescriptor.ActionTicks })
            else
                world |>
                    character.SetCharacterActivityState NoActivity |>
                    character.SetCharacterAnimationState (getCharacterAnimationStateByActionEnd (World.getTickTime world) (character.GetCharacterAnimationState world)) |>
                    tickReaction actionDescriptor character
        
        static let tickUpdate world =
            let enemies = getEnemies world |> Seq.toList
            let characters = Simulants.Player :: enemies
            let rec recursion (characters : Entity list) world =
                if characters.Length = 0 then world
                else
                    let character = characters.Head
                    let world =
                        match character.GetCharacterActivityState world with
                        | Action actionDescriptor ->
                            tickAction actionDescriptor character world
                        | Navigation navigationDescriptor ->
                            if navigationDescriptor.LastWalkOriginM = navigationDescriptor.WalkDescriptor.WalkOriginM then
                                tickNavigation navigationDescriptor character world
                            else world
                        | NoActivity -> world
                    recursion characters.Tail world
            recursion characters world
        
        static let tickNewTurn newPlayerTurn world =

            let occupationMap =
                let fieldMap = Simulants.Field.GetFieldMapNp world
                let enemies = getEnemies world
                let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
                OccupationMap.makeFromFieldTilesAndCharactersAndDesiredTurn fieldMap.FieldTiles enemyPositions newPlayerTurn
            
            let newPlayerActivity =
                match newPlayerTurn with
                | ActionTurn actionDescriptor -> Action actionDescriptor
                | NavigationTurn navigationDescriptor -> Navigation navigationDescriptor
                | CancelTurn -> NoActivity
                | NoTurn -> failwith "newPlayerTurn cannot be NoTurn at this point."
            
            let world = setCharacterActivity newPlayerActivity Simulants.Player world

            // determine (and set) enemy desired turns if applicable
            let world =
                match newPlayerActivity with
                | Action _
                | Navigation _ ->
                    let enemies = getEnemies world
                    let enemyDesiredTurns = determineDesiredEnemyTurns occupationMap Simulants.Player enemies world
                    Seq.fold2 (fun world (enemy : Entity) turn -> enemy.SetDesiredTurn turn world) world enemies enemyDesiredTurns
                | NoActivity -> world

            world
        
        static let tickTurn newPlayerTurnOpt world =

            let playerTurn =
                match newPlayerTurnOpt with
                | Some playerTurn -> playerTurn
                | None -> determinePlayerTurnFromNavigationProgress world

            let world =
                match playerTurn with
                | NoTurn -> world
                | _ -> tickNewTurn playerTurn world

            // set enemy activities in accordance with the player's current activity
            let world =
                let enemies = getEnemies world
                match Simulants.Player.GetCharacterActivityState world with
                | Action _ -> world
                | Navigation _ 
                | NoActivity ->
                    let newEnemyActivities = determineEnemyActivities enemies world
                    if List.exists (fun (state : CharacterActivityState) -> state.IsActing) newEnemyActivities then
                        let world = setEnemyActivities newEnemyActivities enemies world
                        cancelNavigation Simulants.Player world
                    else setEnemyActivities newEnemyActivities enemies world

            let world = tickUpdate world
            
            world

        static let handlePlayerInput playerInput world =
            if (anyTurnsInProgress world) then world
            else
                let world = Simulants.HudSaveGame.SetEnabled false world
                let playerTurn = Some (determinePlayerTurnFromInput playerInput world)
                tickTurn playerTurn world

        static let tick world =
            let world =
                if not (anyTurnsInProgress world) && KeyboardState.isKeyDown KeyboardKey.Up then handlePlayerInput (DetailInput Upward) world
                elif not (anyTurnsInProgress world) && KeyboardState.isKeyDown KeyboardKey.Right then handlePlayerInput (DetailInput Rightward) world
                elif not (anyTurnsInProgress world) && KeyboardState.isKeyDown KeyboardKey.Down then handlePlayerInput (DetailInput Downward) world
                elif not (anyTurnsInProgress world) && KeyboardState.isKeyDown KeyboardKey.Left then handlePlayerInput (DetailInput Leftward) world
                elif (anyTurnsInProgress world) then tickTurn None world
                elif not (Simulants.HudSaveGame.GetEnabled world) then Simulants.HudSaveGame.SetEnabled true world
                else world
            world
        
        static let runNewGameplay world =

            // generate non-deterministic random numbers
            let sysrandom = System.Random ()
            let contentSeedState = uint64 (sysrandom.Next ())

            // initialize gameplay screen
            let world = Simulants.Gameplay.SetContentRandState contentSeedState world

            // make scene layer
            let (scene, world) = World.createLayer (Some Simulants.Scene.Name) Simulants.Gameplay world

            // make rand from gameplay
            let rand = Rand.makeFromSeedState (Simulants.Gameplay.GetContentRandState world)

            // make field
            let (rand, world) = _bc (createField scene rand world)

            // make player
            let (player, world) = World.createEntity<PlayerDispatcher> (Some Simulants.Player.Name) DefaultOverlay scene world
            let world = player.SetDepth Constants.Layout.CharacterDepth world

            // make enemies
            __c (createEnemies scene rand world)

        static let runLoadGameplay world = // TODO: fix it

            // get and initialize gameplay screen from read
            let world = World.readScreenFromFile Assets.SaveFilePath (Some Simulants.Gameplay.Name) world |> snd
            let world = Simulants.Gameplay.SetTransitionState IncomingState world

            // make rand from gameplay
            let rand = Rand.makeFromSeedState (Simulants.Gameplay.GetContentRandState world)

            // make field from rand (field is not serialized, but generated deterministically with ContentRandState)
            __c (createField Simulants.Scene rand world)

        static member Properties =
            [define Screen.ContentRandState Rand.DefaultSeedState
             define Screen.ShallLoadGame false]

        override this.Channel (_, _) =
            [Simulants.Player.CharacterActivityState.ChangeEvent => cmd ToggleHaltButton
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

        override this.Command (_, command, _, world) =
            let world =
                match command with
                | ToggleHaltButton -> Simulants.HudHalt.SetEnabled (isPlayerNavigatingPath world) world
                | HandlePlayerInput input -> handlePlayerInput input world
                | SaveGame gameplay -> World.writeScreenToFile Assets.SaveFilePath gameplay world; world
                | QuittingGameplay -> World.playSong Constants.Audio.DefaultFadeOutMs 1.0f Assets.ButterflyGirlSong world
                | QuitGameplay -> World.destroyLayer Simulants.Scene world
                | RunGameplay ->
                    let world =
                        if Simulants.Gameplay.GetShallLoadGame world && File.Exists Assets.SaveFilePath
                        then runLoadGameplay world
                        else runNewGameplay world
                    World.playSong Constants.Audio.DefaultFadeOutMs 1.0f Assets.HerosVengeanceSong world
                | Tick -> tick world
                | Nop -> world
            just world