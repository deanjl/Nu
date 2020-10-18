namespace InfinityRpg
open System
open System.IO
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module GameplayDispatcher =

    type [<StructuralEquality; NoComparison>] PlayerInput =
        | TouchInput of Vector2
        | DetailInput of Direction
        | NoInput

    type [<StructuralEquality; NoComparison>] GameplayMessage =
        | TryContinuePlayerMove
        | FinishTurns of CharacterIndex list
        | ProgressTurns of CharacterIndex list
        | BeginTurns of CharacterIndex list
        | RunCharacterActivation
        | MakeEnemyMoves
        | TryMakePlayerMove of PlayerInput
        | TransitionMap of Direction
        | HandleMapChange of PlayerInput
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
        
        member this.GetGameplay = this.GetModel<Gameplay>
        member this.SetGameplay = this.SetModel<Gameplay>
        member this.Gameplay = this.Model<Gameplay> ()

    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.initial)

        static let tryGetNavigationPath index positionM gameplay =
            let fieldTiles = gameplay.Field.FieldMapNp.FieldTiles
            let characterPositions = Map.toValueList gameplay.Chessboard.CharacterCoordinates |> List.map (fun positionM -> vmtovf positionM)
            let currentPositionM = Gameplay.getCoordinates PlayerIndex gameplay
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

        override this.Message (gameplay, message, _, world) =
            match message with
            
            | TryContinuePlayerMove ->
                let playerMoveOpt =
                    match gameplay.Player.TurnStatus with
                    | TurnFinishing ->
                        match Gameplay.getCurrentMove PlayerIndex gameplay with
                        | MultiRoundMove move ->
                            match move with
                            | Travel (_ :: navigationPath) ->
                                let targetPositionM = (List.head navigationPath).PositionM
                                if List.exists (fun x -> x = targetPositionM) gameplay.Chessboard.AvailableCoordinates then
                                    Some (MultiRoundMove (Travel navigationPath))
                                else None
                        | _ -> None
                    | _ -> None
             
                let gameplay =
                    match gameplay.Player.TurnStatus with
                    | TurnFinishing -> Gameplay.updateTurnStatus PlayerIndex Idle gameplay
                    | _ -> gameplay
                
                match playerMoveOpt with
                | Some move ->
                    let gameplay = Gameplay.addMove PlayerIndex move gameplay
                    let gameplay = Gameplay.unpackMove PlayerIndex gameplay
                    let gameplay = Gameplay.applyMove PlayerIndex gameplay
                    withMsg MakeEnemyMoves gameplay
                | _ -> just gameplay
            
            | FinishTurns indices ->
                let updater index gameplay =
                    match (Gameplay.getTurnStatus index gameplay) with
                    | TurnFinishing ->
                        match Gameplay.getCharacterActivityState index gameplay with
                        | Action actionDescriptor ->
                            let gameplay = Gameplay.finishMove index gameplay
                            let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                            let reactorState = Gameplay.getCharacterState reactorIndex gameplay
                            let characterAnimationState = Gameplay.getCharacterAnimationState index gameplay
                            let gameplay = Gameplay.updateCharacterAnimationState index (characterAnimationState.Facing (World.getTickTime world)) gameplay
                            if reactorState.HitPoints <= 0 then
                                match reactorIndex with
                                | PlayerIndex -> Gameplay.updateCharacterState reactorIndex {reactorState with ControlType = Uncontrolled} gameplay // TODO: reimplement screen transition
                                | EnemyIndex _ -> Gameplay.removeEnemy reactorIndex gameplay
                            else gameplay
                        | Navigation navigationDescriptor ->
                            let gameplay = Gameplay.setCharacterPositionToCoordinates index gameplay
                            match navigationDescriptor.NavigationPathOpt with
                            | None
                            | Some (_ :: []) -> Gameplay.finishMove index gameplay
                            | _ -> gameplay
                        | _ -> failwith "TurnStatus is TurnFinishing; CharacterActivityState should not be NoActivity"
                    | _ -> failwith "non-finishing turns should be filtered out by this point"

                let gameplay = Gameplay.forEachIndex updater indices gameplay
                withMsg TryContinuePlayerMove gameplay
            
            | ProgressTurns indices ->
                
                let updater index gameplay =
                    match (Gameplay.getTurnStatus index gameplay) with
                    | TurnProgressing ->
                        match Gameplay.getCharacterActivityState index gameplay with
                        | Action actionDescriptor ->
                            let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                            let reactorState = Gameplay.getCharacterState reactorIndex gameplay
                            
                            let gameplay = 
                                if actionDescriptor.ActionTicks = Constants.InfinityRpg.ReactionTick then
                                    if reactorState.HitPoints <= 0 then
                                        let reactorCharacterAnimationState = Gameplay.getCharacterAnimationState reactorIndex gameplay
                                        Gameplay.updateCharacterAnimationState reactorIndex reactorCharacterAnimationState.Slain gameplay
                                    else gameplay
                                else gameplay

                            if actionDescriptor.ActionTicks = Constants.InfinityRpg.ActionTicksMax then
                                Gameplay.updateTurnStatus index TurnFinishing gameplay
                            else Gameplay.updateCharacterActivityState index (Action actionDescriptor.Inc) gameplay
                        | Navigation navigationDescriptor ->
                            let (newPosition, walkState) = Gameplay.getPosition index gameplay |> walk navigationDescriptor.WalkDescriptor
                            let gameplay = Gameplay.updatePosition index newPosition gameplay

                            match walkState with
                            | WalkFinished -> Gameplay.updateTurnStatus index TurnFinishing gameplay
                            | WalkContinuing -> gameplay
                        | NoActivity -> failwith "TurnStatus is TurnProgressing; CharacterActivityState should not be NoActivity"
                    | _ -> gameplay

                let gameplay = Gameplay.forEachIndex updater indices gameplay
                let indices = List.filter (fun x -> (Gameplay.getTurnStatus x gameplay) = TurnFinishing) indices

                withMsg (FinishTurns indices) gameplay
            
            | BeginTurns indices ->

                let updater index gameplay =
                    let characterAnimationState = Gameplay.getCharacterAnimationState index gameplay
                    match (Gameplay.getTurnStatus index gameplay) with
                    | TurnBeginning ->
                        let characterAnimationState =
                            match (Gameplay.getCharacterActivityState index gameplay) with
                            | Action actionDescriptor ->
                                let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                                let characterPosition = Gameplay.getPosition index gameplay
                                let direction = ActionDescriptor.computeActionDirection characterPosition (Gameplay.getCoordinates reactorIndex gameplay)
                                CharacterAnimationState.makeAction (World.getTickTime world) direction
                            | Navigation navigationDescriptor ->
                                characterAnimationState.UpdateDirection navigationDescriptor.WalkDescriptor.WalkDirection
                            | _ -> failwith "TurnStatus is TurnBeginning; CharacterActivityState should not be NoActivity"
                        let gameplay = Gameplay.updateCharacterAnimationState index characterAnimationState gameplay
                        Gameplay.updateTurnStatus index TurnProgressing gameplay // "TurnProgressing" for normal animation; "TurnFinishing" for roguelike mode
                    | _ -> gameplay
                
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                
                withMsg (ProgressTurns indices) gameplay
            
            | RunCharacterActivation ->
                
                // player's turn is converted to activity at the beginning of the round, activating the observable playback of his move
                let gameplay =
                    if gameplay.Player.TurnStatus = TurnPending then
                        Gameplay.activateCharacter PlayerIndex gameplay
                    else gameplay

                // enemies are so activated at the same time during player movement, or after player's action has finished playback
                let indices = Gameplay.getEnemyIndices gameplay |> List.filter (fun x -> (Gameplay.getTurnStatus x gameplay) <> Idle)
                let gameplay =
                    if (List.exists (fun x -> (Gameplay.getTurnStatus x gameplay) = TurnPending) indices) then
                        match gameplay.Player.CharacterActivityState with
                        | Action _ -> gameplay
                        | Navigation _ 
                        | NoActivity -> // TODO: find out why using activateCharacter here triggered "turn status is TurnBeginning..." exception
                            let enemyActivities = Gameplay.getEnemyTurns gameplay |> List.map Turn.toCharacterActivityState
                            let gameplay = Gameplay.forEachIndex (fun index gameplay -> Gameplay.updateTurnStatus index TurnBeginning gameplay) indices gameplay
                            Gameplay.updateEnemyActivityStates enemyActivities gameplay
                    else gameplay
                                
                let indices = List.filter (fun x -> (Gameplay.getTurnStatus x gameplay) <> TurnPending) indices
                
                let indices =
                    match gameplay.Player.TurnStatus with
                    | Idle -> indices
                    | _ -> PlayerIndex :: indices
                
                withMsg (BeginTurns indices) gameplay
            
            | MakeEnemyMoves ->
                
                let indices = Gameplay.getEnemyIndices gameplay
                let attackerOpt = List.tryFind (fun x -> Math.arePositionMsAdjacent (Gameplay.getCoordinates x gameplay) (Gameplay.getCoordinates PlayerIndex gameplay)) indices

                let updater =
                    (fun index gameplay ->
                        let characterState = Gameplay.getCharacterState index gameplay
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
                                        let openDirections = Gameplay.getCoordinates index gameplay |> gameplay.Chessboard.OpenDirections
                                        let direction = Gen.random1 4 |> Direction.fromInt
                                        if List.exists (fun x -> x = direction) openDirections then
                                            Some (SingleRoundMove (Step direction))
                                        else None
                                else None
                            | _ -> None
                        
                        match enemyMoveOpt with
                        | Some move ->
                            let gameplay = Gameplay.addMove index move gameplay
                            let gameplay = Gameplay.unpackMove index gameplay
                            Gameplay.applyMove index gameplay
                        | None -> gameplay)
                        
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                withMsg RunCharacterActivation gameplay

            | TryMakePlayerMove playerInput ->

                let currentCoordinates = Gameplay.getCoordinates PlayerIndex gameplay
                
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
                            let openDirections = gameplay.Chessboard.OpenDirections currentCoordinates
                            let direction = Math.directionToTarget currentCoordinates coordinates
                            let opponents = Gameplay.getOpponentIndices PlayerIndex gameplay
                            if List.exists (fun x -> x = direction) openDirections then
                                Some (SingleRoundMove (Step direction))
                            elif List.exists (fun index -> (Gameplay.getCoordinates index gameplay) = coordinates) opponents then
                                let targetIndex = Gameplay.getIndexByCoordinates coordinates gameplay
                                Some (SingleRoundMove (Attack targetIndex))
                            else None
                        | false ->
                            match tryGetNavigationPath PlayerIndex coordinates gameplay with
                            | Some navigationPath ->
                                match navigationPath with
                                | [] -> None
                                | _ -> Some (MultiRoundMove (Travel navigationPath))
                            | None -> None
                    | None -> None
                
                match playerMoveOpt with
                | Some move ->
                    let gameplay = Gameplay.addMove PlayerIndex move gameplay
                    let gameplay = Gameplay.unpackMove PlayerIndex gameplay
                    let gameplay = Gameplay.applyMove PlayerIndex gameplay
                    withMsg MakeEnemyMoves gameplay
                | _ -> just gameplay

            | TransitionMap direction ->

                let currentCoordinates = Gameplay.getCoordinates PlayerIndex gameplay
                let newCoordinates =
                    match direction with
                    | Upward -> currentCoordinates.WithY 0
                    | Rightward -> currentCoordinates.WithX 0
                    | Downward -> currentCoordinates.WithY (Constants.Layout.FieldUnitSizeM.Y - 1)
                    | Leftward -> currentCoordinates.WithX (Constants.Layout.FieldUnitSizeM.X - 1)
                let gameplay = Gameplay.clearEnemies gameplay
                let gameplay = Gameplay.clearPickups gameplay
                let gameplay = Gameplay.yankPlayer newCoordinates gameplay
                let gameplay = Gameplay.transitionMap direction gameplay
                let fieldMap = gameplay.MapModeler.GetCurrent.ToFieldMap
                let gameplay = Gameplay.setFieldMap fieldMap gameplay
                let gameplay = Gameplay.makeEnemies 1 gameplay
                just gameplay

            | HandleMapChange playerInput ->
                
                let msg =
                    match playerInput with
                    | DetailInput direction ->
                        let currentCoordinates = Gameplay.getCoordinates PlayerIndex gameplay
                        let targetOutside =
                            match direction with
                            | Upward -> currentCoordinates.Y = Constants.Layout.FieldUnitSizeM.Y - 1
                            | Rightward -> currentCoordinates.X = Constants.Layout.FieldUnitSizeM.X - 1
                            | Downward -> currentCoordinates.Y = 0
                            | Leftward -> currentCoordinates.X = 0
                        if targetOutside && gameplay.MapModeler.PossibleInDirection direction then
                            TransitionMap direction
                        else TryMakePlayerMove playerInput
                    | _ -> TryMakePlayerMove playerInput
                withMsg msg gameplay
            
            | StartGameplay -> // TODO: and fix >1 new games again!
                if gameplay.ShallLoadGame && File.Exists Assets.SaveFilePath then
                    let gameplayStr = File.ReadAllText Assets.SaveFilePath
                    let gameplay = scvalue<Gameplay> gameplayStr
                    just gameplay
                else
                    let fieldMap = gameplay.MapModeler.GetCurrent.ToFieldMap
                    let gameplay = Gameplay.setFieldMap fieldMap gameplay
                    let gameplay = Gameplay.makePlayer gameplay
                    let gameplay = Gameplay.makeEnemies 1 gameplay
                    just gameplay

        override this.Command (gameplay, command, _, world) =
            match command with
            | ToggleHaltButton ->
                let world = Simulants.HudHalt.SetEnabled (gameplay.Player.CharacterActivityState.IsNavigatingPath) world
                just world
            | HandlePlayerInput playerInput ->
                if not (Gameplay.anyTurnsInProgress gameplay) then
                    let world = Simulants.HudSaveGame.SetEnabled false world
                    match gameplay.Player.CharacterState.ControlType with
                    | PlayerControlled -> withMsg (HandleMapChange playerInput) world
                    | _ -> just world
                else just world
            | SaveGame -> // TODO: fix save once again when the new map handling system is in place
                let gameplayStr = scstring gameplay
                File.WriteAllText (Assets.SaveFilePath, gameplayStr)
                just world
            | Tick ->
                if (Gameplay.anyTurnsInProgress gameplay) then withMsg RunCharacterActivation world
                elif KeyboardState.isKeyDown KeyboardKey.Up then withCmd (HandlePlayerInput (DetailInput Upward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Right then withCmd (HandlePlayerInput (DetailInput Rightward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Down then withCmd (HandlePlayerInput (DetailInput Downward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Left then withCmd (HandlePlayerInput (DetailInput Leftward)) world
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

        override this.Content (gameplay, screen) =
        
            [Content.layerIfScreenSelected screen (fun _ _ ->
                Content.layer Simulants.Scene.Name []

                    [Content.entity<FieldDispatcher> Simulants.Field.Name
                       [Entity.Field <== gameplay --> fun gameplay -> gameplay.Field]
                                        
                     Content.entities gameplay
                        (fun gameplay -> gameplay.Pickups) constant
                        (fun index pickup _ -> Content.entity<PickupDispatcher> ("Pickup+" + scstring index) [Entity.Pickup <== pickup])
                     
                     Content.entitiesIndexedBy gameplay
                        (fun gameplay -> gameplay.Enemys) constant
                        (fun character -> match character.Index with EnemyIndex i -> i | _ -> failwithumf ())
                        (fun index character _ -> Content.entity<EnemyDispatcher> ("Enemy+" + scstring index) [Entity.Character <== character])

                     Content.entity<PlayerDispatcher> Simulants.Player.Name
                       [Entity.Character <== gameplay --> fun gameplay -> gameplay.Player]])]
