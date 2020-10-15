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
        
        member this.GetGameplayModel = this.GetModel<GameplayModel>
        member this.SetGameplayModel = this.SetModel<GameplayModel>
        member this.GameplayModel = this.Model<GameplayModel> ()

    type GameplayDispatcher () =
        inherit ScreenDispatcher<GameplayModel, GameplayMessage, GameplayCommand> (GameplayModel.initial)

        static let tryGetNavigationPath index positionM model =
            let fieldTiles = model.Field.FieldMapNp.FieldTiles
            let characterPositions = Map.toValueList model.Chessboard.CharacterCoordinates |> List.map (fun positionM -> vmtovf positionM)
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
            
            | TryContinuePlayerMove ->
                let playerMoveOpt =
                    match model.PlayerModel.TurnStatus with
                    | TurnFinishing ->
                        match GameplayModel.getCurrentMove PlayerIndex model with
                        | MultiRoundMove move ->
                            match move with
                            | Travel (_ :: navigationPath) ->
                                let targetPositionM = (List.head navigationPath).PositionM
                                if List.exists (fun x -> x = targetPositionM) model.Chessboard.AvailableCoordinates then
                                    Some (MultiRoundMove (Travel navigationPath))
                                else None
                        | _ -> None
                    | _ -> None
             
                let model =
                    match model.PlayerModel.TurnStatus with
                    | TurnFinishing -> GameplayModel.updateTurnStatus PlayerIndex Idle model
                    | _ -> model
                
                match playerMoveOpt with
                | Some move ->
                    let model = GameplayModel.addMove PlayerIndex move model
                    let model = GameplayModel.unpackMove PlayerIndex model
                    let model = GameplayModel.applyMove PlayerIndex model
                    withMsg model MakeEnemyMoves
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
                            let model = GameplayModel.setCharacterPositionToCoordinates index model
                            match navigationDescriptor.NavigationPathOpt with
                            | None
                            | Some (_ :: []) -> GameplayModel.finishMove index model
                            | _ -> model
                        | _ -> failwith "TurnStatus is TurnFinishing; CharacterActivityState should not be NoActivity"
                    | _ -> failwith "non-finishing turns should be filtered out by this point"

                let model = GameplayModel.forEachIndex updater indices model
                withMsg model TryContinuePlayerMove
            
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
                    if model.PlayerModel.TurnStatus = TurnPending then
                        GameplayModel.activateCharacter PlayerIndex model
                    else model

                // enemies are so activated at the same time during player movement, or after player's action has finished playback
                let indices = GameplayModel.getEnemyIndices model |> List.filter (fun x -> (GameplayModel.getTurnStatus x model) <> Idle)
                let model =
                    if (List.exists (fun x -> (GameplayModel.getTurnStatus x model) = TurnPending) indices) then
                        match model.PlayerModel.CharacterActivityState with
                        | Action _ -> model
                        | Navigation _ 
                        | NoActivity -> // TODO: find out why using activateCharacter here triggered "turn status is TurnBeginning..." exception
                            let enemyActivities = GameplayModel.getEnemyTurns model |> List.map Turn.toCharacterActivityState
                            let model = GameplayModel.forEachIndex (fun index model -> GameplayModel.updateTurnStatus index TurnBeginning model) indices model
                            GameplayModel.updateEnemyActivityStates enemyActivities model
                    else model
                                
                let indices = List.filter (fun x -> (GameplayModel.getTurnStatus x model) <> TurnPending) indices
                
                let indices =
                    match model.PlayerModel.TurnStatus with
                    | Idle -> indices
                    | _ -> PlayerIndex :: indices
                
                withMsg model (BeginTurns indices)
            
            | MakeEnemyMoves ->
                
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
                                        let openDirections = GameplayModel.getCoordinates index model |> model.Chessboard.OpenDirections
                                        let direction = Gen.random1 4 |> Direction.fromInt
                                        if List.exists (fun x -> x = direction) openDirections then
                                            Some (SingleRoundMove (Step direction))
                                        else None
                                else None
                            | _ -> None
                        
                        match enemyMoveOpt with
                        | Some move ->
                            let model = GameplayModel.addMove index move model
                            let model = GameplayModel.unpackMove index model
                            GameplayModel.applyMove index model
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
                            let openDirections = model.Chessboard.OpenDirections currentCoordinates
                            let direction = directionToTarget currentCoordinates coordinates
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
                | Some move ->
                    let model = GameplayModel.addMove PlayerIndex move model
                    let model = GameplayModel.unpackMove PlayerIndex model
                    let model = GameplayModel.applyMove PlayerIndex model
                    withMsg model MakeEnemyMoves
                | _ -> just model

            | TransitionMap direction ->

                let currentCoordinates = GameplayModel.getCoordinates PlayerIndex model
                let newCoordinates =
                    match direction with
                    | Upward -> currentCoordinates.WithY 0
                    | Rightward -> currentCoordinates.WithX 0
                    | Downward -> currentCoordinates.WithY (Constants.Layout.FieldUnitSizeM.Y - 1)
                    | Leftward -> currentCoordinates.WithX (Constants.Layout.FieldUnitSizeM.X - 1)
                let model = GameplayModel.clearEnemies model
                let model = GameplayModel.clearPickups model
                let model = GameplayModel.yankPlayer newCoordinates model
                let model = GameplayModel.transitionMap direction model
                let fieldMap = model.MapModeler.GetCurrent.ToFieldMap
                let model = GameplayModel.setFieldMap fieldMap model
                let model = GameplayModel.makeEnemies 3 model
                just model

            | HandleMapChange playerInput ->
                
                let msg =
                    match playerInput with
                    | DetailInput direction ->
                        let currentCoordinates = GameplayModel.getCoordinates PlayerIndex model
                        let targetOutside =
                            match direction with
                            | Upward -> currentCoordinates.Y = Constants.Layout.FieldUnitSizeM.Y - 1
                            | Rightward -> currentCoordinates.X = Constants.Layout.FieldUnitSizeM.X - 1
                            | Downward -> currentCoordinates.Y = 0
                            | Leftward -> currentCoordinates.X = 0
                        if targetOutside && model.MapModeler.PossibleInDirection direction then
                            TransitionMap direction
                        else TryMakePlayerMove playerInput
                    | _ -> TryMakePlayerMove playerInput
                withMsg model msg
            
            | StartGameplay -> // TODO: and fix >1 new games again!
                if model.ShallLoadGame && File.Exists Assets.SaveFilePath then
                    let modelStr = File.ReadAllText Assets.SaveFilePath
                    let model = scvalue<GameplayModel> modelStr
                    just model
                else
                    let fieldMap = model.MapModeler.GetCurrent.ToFieldMap
                    let model = GameplayModel.setFieldMap fieldMap model
                    let model = GameplayModel.makePlayer model
                    let model = GameplayModel.makeEnemies 4 model
                    just model

        override this.Command (model, command, _, world) =
            match command with
            | ToggleHaltButton ->
                let world = Simulants.HudHalt.SetEnabled (model.PlayerModel.CharacterActivityState.IsNavigatingPath) world
                just world
            | HandlePlayerInput playerInput ->
                if not (GameplayModel.anyTurnsInProgress model) then
                    let world = Simulants.HudSaveGame.SetEnabled false world
                    match model.PlayerModel.CharacterState.ControlType with
                    | PlayerControlled -> withMsg world (HandleMapChange playerInput)
                    | _ -> just world
                else just world
            | SaveGame -> // TODO: fix save once again when the new map handling system is in place
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

                    [Content.entity<FieldDispatcher> Simulants.Field.Name
                       [Entity.FieldModel <== model --> fun model -> model.Field]
                                        
                     Content.entities model
                        (fun model -> model.PickupModels) constant
                        (fun index model _ -> Content.entity<PickupDispatcher> ("Pickup+" + scstring index) [Entity.PickupModel <== model])
                     
                     Content.entitiesIndexedBy model
                        (fun model -> model.EnemyModels) constant
                        (fun model -> model.Index.getEnemyIndex)
                        (fun index model _ -> Content.entity<EnemyDispatcher> ("Enemy+" + scstring index) [Entity.CharacterModel <== model])

                     Content.entity<PlayerDispatcher> Simulants.Player.Name
                       [Entity.CharacterModel <== model --> fun model -> model.PlayerModel]])]
