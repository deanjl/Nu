﻿namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type [<CustomEquality; CustomComparison>] Advent =
    | SavedPrincess
    | FoughtBaddie of bool

    member private this.ToInt () =
        match this with
        | SavedPrincess -> 0
        | FoughtBaddie _ -> 1

    override this.GetHashCode () =
        let rand = Rand.makeFromInt (this.ToInt ())
        let (result, _) = Rand.nextInt rand
        result

    override this.Equals that =
        match that with
        | :? Advent as thatAdvent -> (this :> IComparable<Advent>).CompareTo thatAdvent = 0
        | _ -> false

    interface IComparable<Advent> with
        member this.CompareTo that =
            let thisInt = this.ToInt ()
            let thatInt = that.ToInt ()
            thisInt.CompareTo thatInt
            
    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? Advent as thatAdvent -> (this :> IComparable<Advent>).CompareTo thatAdvent
            | _ -> -1

type Inventory =
    { Items : Map<ItemType, int> }

    static member getKeyItems inv =
        inv.Items |>
        Map.toArray |>
        Array.map (function (KeyItem keyItemType, count) -> Some (keyItemType, count) | _ -> None) |>
        Array.definitize |>
        Map.ofArray
        
    static member getConsumables inv =
        inv.Items |>
        Map.toArray |>
        Array.map (function (Consumable consumableType, count) -> Some (consumableType, count) | _ -> None) |>
        Array.definitize |>
        Map.ofArray

    static member containsItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 0 -> true
        | _ -> false

    static member removeItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 1 ->
            { inventory with Items = Map.add item (dec itemCount) inventory.Items }
        | Some itemCount when itemCount = 1 ->
            { inventory with Items = Map.remove item inventory.Items }
        | _ -> inventory

type CharacterIndex =
    | AllyIndex of int
    | EnemyIndex of int

    static member isTeammate index index2 =
        match (index, index2) with
        | (AllyIndex _, AllyIndex _) -> true
        | (EnemyIndex _, EnemyIndex _) -> true
        | (_, _) -> false

type AutoBattle =
    { AutoTarget : CharacterIndex
      AutoTechOpt : TechType option }

/// The state of a character.
/// Used both inside and outside of battle.
/// Level is calculated from base experience + added experience.
type CharacterState =
    { CharacterIndex : CharacterIndex // key
      ArchetypeType : ArchetypeType
      ExpPoints : int
      HitPoints : int
      TechPoints : int
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Accessories : AccessoryType list
      Statuses : StatusType Set
      Defending : bool
      Charging : bool
      PowerBuff : single
      ShieldBuff : single
      MagicBuff : single
      CounterBuff : single
      ActionTime : int
      AutoBattleOpt : AutoBattle option
      AnimationSheet : Image AssetTag }

    member this.PartyIndex = match this.CharacterIndex with AllyIndex index | EnemyIndex index -> index
    member this.IsAlly = match this.CharacterIndex with AllyIndex _ -> true | EnemyIndex _ -> false
    member this.IsEnemy = not this.IsAlly
    member this.IsHealthy = this.HitPoints > 0
    member this.IsWounded = this.HitPoints <= 0
    member this.Level = Algorithms.expPointsToLevel this.ExpPoints
    member this.HitPointsMax = Algorithms.hitPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.TechPointsMax = Algorithms.techPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.Power = Algorithms.power this.WeaponOpt this.PowerBuff this.ArchetypeType this.Level
    member this.Magic = Algorithms.magic this.WeaponOpt this.MagicBuff this.ArchetypeType this.Level
    member this.Shield effectType = Algorithms.shield effectType this.Accessories this.ShieldBuff this.ArchetypeType this.Level
    member this.Techs = Algorithms.techs this.ArchetypeType this.Level

    static member isTeammate (state : CharacterState) (state2 : CharacterState) =
        CharacterIndex.isTeammate state.CharacterIndex state2.CharacterIndex
        
    static member runningTechAutoBattle state =
        match state.AutoBattleOpt with
        | Some autoBattle -> Option.isSome autoBattle.AutoTechOpt
        | None -> false

    static member getAttackResult effectType (source : CharacterState) (target : CharacterState) =
        let power = source.Power
        let shield = target.Shield effectType
        let damageUnscaled = power - shield
        let damage = single damageUnscaled |> int |> max 1
        damage

    static member updateActionTime updater state =
        { state with ActionTime = updater state.ActionTime }

    static member updateAutoBattleOpt updater state =
        { state with AutoBattleOpt = updater state.AutoBattleOpt }

    static member updateHitPoints updater (state : CharacterState) =
        let (hitPoints, cancel) = updater state.HitPoints
        let hitPoints = max 0 hitPoints
        let hitPoints = min state.HitPointsMax hitPoints
        let autoBattleOpt = 
            match state.AutoBattleOpt with
            | Some autoBattle when cancel -> Some { autoBattle with AutoTechOpt = None }
            | _ -> None
        { state with HitPoints = hitPoints; AutoBattleOpt = autoBattleOpt }

    static member updateTechPoints updater state =
        let specialPoints = updater state.TechPoints
        let specialPoints = max 0 specialPoints
        let specialPoints = min state.TechPointsMax specialPoints
        { state with TechPoints = specialPoints }

    static member tryGetTechRandom (state : CharacterState) =
        let specials = state.Techs
        if Set.notEmpty specials then
            let specialIndex = Gen.random1 specials.Count
            let special = Seq.item specialIndex specials
            Some special
        else None

    static member getPoiseType state =
        if state.Defending then Defending
        elif state.Charging then Charging
        else Poising

    static member make characterIndex characterType expPoints weaponOpt armorOpt accessories animationSheet =
        let (archetypeType, levelBase) =
            match Map.tryFind characterType data.Value.Characters with
            | Some characterData -> (characterData.ArchetypeType, characterData.LevelBase)
            | None -> (Squire, 0)
        let expPointsTotal = Algorithms.levelToExpPoints levelBase + expPoints
        let level = Algorithms.expPointsToLevel expPointsTotal
        let hitPointsMax = Algorithms.hitPointsMax armorOpt archetypeType level
        let techPointsMax = Algorithms.hitPointsMax armorOpt archetypeType level
        let characterState =
            { CharacterIndex = characterIndex
              ArchetypeType = archetypeType
              ExpPoints = expPointsTotal
              HitPoints = hitPointsMax
              TechPoints = techPointsMax
              WeaponOpt = weaponOpt
              ArmorOpt = armorOpt
              Accessories = accessories
              Statuses = Set.empty
              Defending = false
              Charging = false
              PowerBuff = 1.0f
              MagicBuff = 1.0f
              ShieldBuff = 1.0f
              CounterBuff = 1.0f
              ActionTime = 0
              AutoBattleOpt = None
              AnimationSheet = animationSheet }
        characterState

type CharacterAnimationState =
    { TimeStart : int64
      AnimationSheet : Image AssetTag
      AnimationCycle : CharacterAnimationCycle
      Direction : Direction }

    static member setCycle timeOpt cycle state =
        if state.AnimationCycle <> cycle then
            match timeOpt with
            | Some time -> { state with TimeStart = time; AnimationCycle = cycle }
            | None -> { state with AnimationCycle = cycle }
        else state

    static member directionToInt direction =
        match direction with
        | Downward -> 0
        | Leftward -> 1
        | Upward -> 2
        | Rightward -> 3

    static member timeLocal time state =
        time - state.TimeStart

    static member indexCel stutter time state =
        let timeLocal = CharacterAnimationState.timeLocal time state
        int (timeLocal / stutter)

    static member indexLooped run stutter time state =
        CharacterAnimationState.indexCel stutter time state % run

    static member indexSaturated run stutter time state =
        let cel = CharacterAnimationState.indexCel stutter time state
        if cel < dec run then cel else dec run

    static member indexLoopedWithDirection run stutter offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexLooped run stutter time state + position, 0)
        let position = position + offset
        position

    static member indexLoopedWithoutDirection run stutter offset time state =
        let position = CharacterAnimationState.indexLooped run stutter time state
        let position = v2i position 0 + offset
        position

    static member indexSaturatedWithDirection run stutter offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position =  Vector2i (CharacterAnimationState.indexSaturated run stutter time state + position, 0)
        let position = position + offset
        position

    static member indexSaturatedWithoutDirection run stutter offset time state =
        let position = CharacterAnimationState.indexSaturated run stutter time state
        let position = Vector2i (position, 0)
        let position = position + offset
        position

    static member index time state =
        match Map.tryFind state.AnimationCycle data.Value.CharacterAnimations with
        | Some animationData ->
            match animationData.AnimationType with
            | LoopedWithDirection -> CharacterAnimationState.indexLoopedWithDirection animationData.Run animationData.Stutter animationData.Offset time state
            | LoopedWithoutDirection -> CharacterAnimationState.indexLoopedWithoutDirection animationData.Run animationData.Stutter animationData.Offset time state
            | SaturatedWithDirection -> CharacterAnimationState.indexSaturatedWithDirection animationData.Run animationData.Stutter animationData.Offset time state
            | SaturatedWithoutDirection -> CharacterAnimationState.indexSaturatedWithoutDirection animationData.Run animationData.Stutter animationData.Offset time state
        | None -> v2iZero

    static member progressOpt time state =
        match Map.tryFind state.AnimationCycle data.Value.CharacterAnimations with
        | Some animationData ->
            let timeLocal = CharacterAnimationState.timeLocal time state
            match animationData.LengthOpt with
            | Some length -> Some (min 1.0f (single timeLocal / single length))
            | None -> None
        | None -> None

    static member getFinished time state =
        match CharacterAnimationState.progressOpt time state with
        | Some progress -> progress = 1.0f
        | None -> false

type CharacterInputState =
    | NoInput
    | RegularMenu
    | TechMenu
    | ItemMenu
    | AimReticles of string * AimType

    member this.AimType =
        match this with
        | NoInput | RegularMenu | TechMenu | ItemMenu -> NoAim
        | AimReticles (_, aimType) -> aimType