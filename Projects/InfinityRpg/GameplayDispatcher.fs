﻿namespace InfinityRpg
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
        | TryMakePlayerHalt
        | TransitionMap of Direction
        | HandleMapChange of PlayerInput
        | StartGameplay

    type [<NoEquality; NoComparison>] GameplayCommand =
        | HandlePlayerInput of PlayerInput
        | SaveGame
        | Tick
        | PostTick
        | Nop

    type Screen with
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
            [Simulants.Gameplay.SelectEvent => msg StartGameplay
             Simulants.Gameplay.UpdateEvent => cmd Tick
             Simulants.Gameplay.PostUpdateEvent => cmd PostTick]

        override this.Message (gameplay, message, _, world) =
            
            match message with
            | TryContinuePlayerMove ->
                let playerMoveOpt =
                    match gameplay.Player.TurnStatus with
                    | TurnFinishing ->
                        match Gameplay.getCurrentMove PlayerIndex gameplay with
                        | MultiRoundMove path ->
                            match path with
                            | _ :: navigationPath ->
                                let targetPositionM = (List.head navigationPath).PositionM
                                if List.exists (fun x -> x = targetPositionM) gameplay.Chessboard.AvailableCoordinates
                                then Some (MultiRoundMove navigationPath)
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
                            | None | Some (_ :: []) -> Gameplay.finishMove index gameplay
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
                            if actionDescriptor.ActionTicks = Constants.InfinityRpg.ActionTicksMax
                            then Gameplay.updateTurnStatus index TurnFinishing gameplay
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
                let gameplay = // player's turn is converted to activity at the beginning of the round, activating the observable playback of his move
                    if gameplay.Player.TurnStatus = TurnPending then Gameplay.activateCharacter PlayerIndex gameplay else gameplay
                let indices = // enemies are activated at the same time during player movement, or after player's action has finished playback
                    Gameplay.getEnemyIndices gameplay |> List.filter (fun x -> (Gameplay.getTurnStatus x gameplay) <> Idle)
                let gameplay =
                    if (List.exists (fun x -> (Gameplay.getTurnStatus x gameplay) = TurnPending) indices) then
                        match gameplay.Player.CharacterActivityState with
                        | Action _ -> gameplay
                        | Navigation _ 
                        | NoActivity -> // TODO: find out why using activateCharacter here triggered "turn status is TurnBeginning..." exception
                            let enemyActivities = Gameplay.getEnemyTurns gameplay |> List.map CharacterActivityState.makeFromTurn
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
                        if Math.arePositionMsAdjacent coordinates currentCoordinates then
                            let openDirections = gameplay.Chessboard.OpenDirections currentCoordinates
                            let direction = Math.directionToTarget currentCoordinates coordinates
                            let opponents = Gameplay.getOpponentIndices PlayerIndex gameplay
                            if List.exists (fun x -> x = direction) openDirections
                            then Some (SingleRoundMove (Step direction))
                            elif List.exists (fun index -> (Gameplay.getCoordinates index gameplay) = coordinates) opponents then
                                let targetIndex = Gameplay.getIndexByCoordinates coordinates gameplay
                                Some (SingleRoundMove (Attack targetIndex))
                            else None
                        else
                            match tryGetNavigationPath PlayerIndex coordinates gameplay with
                            | Some navigationPath ->
                                match navigationPath with
                                | [] -> None
                                | _ -> Some (MultiRoundMove navigationPath)
                            | None -> None
                    | None -> None
                
                match playerMoveOpt with
                | Some move ->
                    let gameplay = Gameplay.addMove PlayerIndex move gameplay
                    let gameplay = Gameplay.unpackMove PlayerIndex gameplay
                    let gameplay = Gameplay.applyMove PlayerIndex gameplay
                    withMsg MakeEnemyMoves gameplay
                | _ -> just gameplay

            | TryMakePlayerHalt ->
                match Map.tryFind PlayerIndex gameplay.Chessboard.CurrentMoves with
                | Some move ->
                    match move with
                    | MultiRoundMove path when List.hasAtLeast 2 path ->
                        let gameplay = Gameplay.addMove PlayerIndex (MultiRoundMove (List.take 2 path)) gameplay
                        just gameplay
                    | _ -> just gameplay
                | None -> just gameplay

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
                let gameplay = Gameplay.setFieldMap (FieldMap.makeFromFieldMapUnit gameplay.MapModeler.Current) gameplay
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
            
            | StartGameplay ->
                if gameplay.ShallLoadGame && File.Exists Assets.SaveFilePath then
                    let gameplayStr = File.ReadAllText Assets.SaveFilePath
                    let gameplay = scvalue<Gameplay> gameplayStr
                    just gameplay
                else
                    let gameplay = Gameplay.initial
                    let gameplay = Gameplay.setFieldMap (FieldMap.makeFromFieldMapUnit gameplay.MapModeler.Current) gameplay
                    let gameplay = Gameplay.makePlayer gameplay
                    let gameplay = Gameplay.makeEnemies 1 gameplay
                    just gameplay

        override this.Command (gameplay, command, _, world) =

            match command with
            | HandlePlayerInput playerInput ->
                if not (Gameplay.anyTurnsInProgress gameplay) then
                    match gameplay.Player.CharacterState.ControlType with
                    | PlayerControlled -> withMsg (HandleMapChange playerInput) world
                    | _ -> just world
                else just world

            | SaveGame ->
                let gameplayWithoutMoves = { gameplay with Chessboard = { gameplay.Chessboard with CurrentMoves = Map.empty }}
                let gameplayStr = scstring gameplayWithoutMoves
                File.WriteAllText (Assets.SaveFilePath, gameplayStr)
                just world

            | Tick ->
                if (Gameplay.anyTurnsInProgress gameplay) then withMsg RunCharacterActivation world
                elif KeyboardState.isKeyDown KeyboardKey.Up then withCmd (HandlePlayerInput (DetailInput Upward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Right then withCmd (HandlePlayerInput (DetailInput Rightward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Down then withCmd (HandlePlayerInput (DetailInput Downward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Left then withCmd (HandlePlayerInput (DetailInput Leftward)) world
                else just world

            | PostTick ->
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

            | Nop ->
                just world

        override this.Content (gameplay, screen) =

            // scene layer
            [Content.layerIfScreenSelected screen (fun _ _ ->
                Content.layer Simulants.Scene.Name []

                    [Content.entity<FieldDispatcher> Simulants.Field.Name
                       [Entity.Field <== gameplay --> fun gameplay -> gameplay.Field]

                     Content.entities gameplay
                        (fun gameplay -> gameplay.Pickups) constant
                        (fun index pickup _ -> Content.entity<PickupDispatcher> ("Pickup+" + scstring index) [Entity.Pickup <== pickup])

                     Content.entitiesIndexedBy gameplay
                        (fun gameplay -> gameplay.Enemies) constant
                        (fun character -> match character.Index with EnemyIndex i -> i | _ -> failwithumf ())
                        (fun index character _ -> Content.entity<EnemyDispatcher> ("Enemy+" + scstring index) [Entity.Character <== character])

                     Content.entity<PlayerDispatcher> Simulants.Player.Name
                       [Entity.Character <== gameplay --> fun gameplay -> gameplay.Player]])

             // hud layer
             Content.layer Simulants.Hud.Name []

                [Content.button Simulants.HudSaveGame.Name
                    [Entity.Position == v2 88.0f -184.0f; Entity.Size == v2 384.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "SaveGameUp"; Entity.DownImage == asset "Gui" "SaveGameDown"
                     Entity.Enabled <== gameplay --> fun gameplay -> not (Gameplay.anyTurnsInProgress gameplay)
                     Entity.ClickEvent ==> cmd SaveGame]

                 Content.button Simulants.HudHalt.Name
                    [Entity.Position == v2 88.0f -112.0f; Entity.Size == v2 384.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "HaltUp"; Entity.DownImage == asset "Gui" "HaltDown"
                     Entity.Enabled <== gameplay --> fun gameplay -> match gameplay.Player.Turn with NavigationTurn _ -> true | _ -> false
                     Entity.ClickEvent ==> msg TryMakePlayerHalt]

                 Content.button Simulants.HudBack.Name
                    [Entity.Position == v2 88.0f -256.0f; Entity.Size == v2 384.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "BackUp"; Entity.DownImage == asset "Gui" "BackDown"]

                 Content.label Gen.name
                    [Entity.Position == v2 -448.0f -240.0f; Entity.Size == v2 224.0f 224.0f; Entity.Depth == 9.0f
                     Entity.LabelImage == asset "Gui" "DetailBacking"]

                 Content.button Simulants.HudDetailUpward.Name
                    [Entity.Position == v2 -368.0f -88.0f; Entity.Size == v2 64.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "DetailUpwardUp"; Entity.DownImage == asset "Gui" "DetailUpwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Upward))]

                 Content.button Simulants.HudDetailRightward.Name
                    [Entity.Position == v2 -296.0f -160.0f; Entity.Size == v2 64.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "DetailRightwardUp"; Entity.DownImage == asset "Gui" "DetailRightwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Rightward))]

                 Content.button Simulants.HudDetailDownward.Name
                    [Entity.Position == v2 -368.0f -232.0f; Entity.Size == v2 64.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "DetailDownwardUp"; Entity.DownImage == asset "Gui" "DetailDownwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Downward))]

                 Content.button Simulants.HudDetailLeftward.Name
                    [Entity.Position == v2 -440.0f -160.0f; Entity.Size == v2 64.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "DetailLeftwardUp"; Entity.DownImage == asset "Gui" "DetailLeftwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Leftward))]

                 Content.feeler Simulants.HudFeeler.Name
                    [Entity.Position == v2 -480.0f -272.0f; Entity.Size == v2 960.0f 544.0f; Entity.Depth == 9.0f
                     Entity.TouchEvent ==|> fun evt -> cmd (HandlePlayerInput (TouchInput evt.Data))]]]
