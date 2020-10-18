﻿namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type [<NoComparison>] DialogForm =
    | DialogThin
    | DialogMedium
    | DialogLarge

type [<ReferenceEquality; NoComparison>] Dialog =
    { DialogForm : DialogForm
      DialogText : string
      DialogProgress : int
      DialogPage : int }

type [<ReferenceEquality; NoComparison>] SubmenuUse =
    { SubmenuUseSelection : int * ItemType
      SubmenuUseLine1 : string
      SubmenuUseLine2 : string }

    static member make selection line1 line2 =
        { SubmenuUseSelection = selection
          SubmenuUseLine1 = line1
          SubmenuUseLine2 = line2 }

    static member makeFromConsumableData selection (cd : ConsumableData) =
        let prompt = "Use " + string cd.ConsumableType + " on whom?"
        let effect = "(Effect: " + cd.Description + ")"
        SubmenuUse.make selection prompt effect

    static member makeFromWeaponData selection (wd : WeaponData) =
        let prompt = "Equip " + wd.WeaponType + " to whom?"
        let stats = "(Pow: " + string wd.PowerBase + " | Mag: " + string wd.MagicBase + ")"
        SubmenuUse.make selection prompt stats

    static member makeFromArmorData selection (ad : ArmorData) =
        let prompt = "Equip " + ad.ArmorType + " to whom?"
        let stats = "(HP: " + string ad.HitPointsBase + " | TP: " + string ad.TechPointsBase + ")"
        SubmenuUse.make selection prompt stats

    static member makeFromAccessoryData selection (ad : AccessoryData) =
        let prompt = "Equip " + ad.AccessoryType + " to whom?"
        let stats = "(Blk: " + string ad.ShieldBase + " | Ctr: " + string ad.CounterBase + ")"
        SubmenuUse.make selection prompt stats

    static member tryMakeFromSelection selection =
        match snd selection with
        | Consumable ty ->
            match Map.tryFind ty data.Value.Consumables with
            | Some cd -> SubmenuUse.makeFromConsumableData selection cd |> Some
            | None -> None
        | Equipment ty ->
            match ty with
            | WeaponType name ->
                match Map.tryFind name data.Value.Weapons with
                | Some wd -> SubmenuUse.makeFromWeaponData selection wd |> Some
                | None -> None
            | ArmorType name ->
                match Map.tryFind name data.Value.Armors with
                | Some ad -> SubmenuUse.makeFromArmorData selection ad |> Some
                | None -> None
            | AccessoryType name ->
                match Map.tryFind name data.Value.Accessories with
                | Some ad -> SubmenuUse.makeFromAccessoryData selection ad |> Some
                | None -> None
        | KeyItem _ | Stash _ -> None

type [<ReferenceEquality; NoComparison>] SubmenuLegion =
    { LegionIndex : int
      LegionIndices : int list }
      
    static member tryGetLegionnaire (legion : Legion) submenuLegion =
        Map.tryFind submenuLegion.LegionIndex legion

    static member tryGetLegionnaireAndLegionData legion submenuLegion =
        match SubmenuLegion.tryGetLegionnaire legion submenuLegion with
        | Some legionnaire ->
            match Map.tryFind legionnaire.CharacterType data.Value.Characters with
            | Some characterData -> Some (legionnaire, characterData)
            | None -> None
        | None -> None

    static member tryGetLegionData legion submenuLegion =
        let lacdOpt = SubmenuLegion.tryGetLegionnaireAndLegionData legion submenuLegion
        Option.map snd lacdOpt

type [<ReferenceEquality; NoComparison>] SubmenuItem =
    { ItemPage : int }

type [<NoComparison>] SubmenuState =
    | SubmenuLegion of SubmenuLegion
    | SubmenuItem of SubmenuItem
    | SubmenuClosed

type [<ReferenceEquality; NoComparison>] Submenu =
    { SubmenuState : SubmenuState
      SubmenuUseOpt : SubmenuUse option }

type ShopState =
    | ShopBuying
    | ShopSelling

type [<ReferenceEquality; NoComparison>] ShopConfirm =
    { ShopConfirmSelection : int * ItemType
      ShopConfirmPrice : int
      ShopConfirmOffer : string
      ShopConfirmLine1 : string
      ShopConfirmLine2 : string }

    static member make selection price offer line1 line2 =
        { ShopConfirmSelection = selection
          ShopConfirmPrice = price
          ShopConfirmOffer = offer
          ShopConfirmLine1 = line1
          ShopConfirmLine2 = line2 }

    static member makeFromConsumableData buying inventory selection cd =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then cd.Cost else cd.Cost / 2
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let effect = "Effect: " + cd.Description
        let stats = "Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer stats effect

    static member makeFromWeaponData buying inventory selection (wd : WeaponData) =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then wd.Cost else wd.Cost / 2
        let effect = "Effect: " + wd.Description
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let stats = "Pow: " + string wd.PowerBase + " | Mag: " + string wd.MagicBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer stats effect

    static member makeFromArmorData buying inventory selection (ad : ArmorData) =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then ad.Cost else ad.Cost / 2
        let effect = "Effect: " + ad.Description
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let stats = "HP: " + string ad.HitPointsBase + " | TP: " + string ad.TechPointsBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer stats effect

    static member makeFromAccessoryData buying inventory selection (ad : AccessoryData) =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then ad.Cost else ad.Cost / 2
        let effect = "Effect: " + ad.Description
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let stats = "Blk: " + string ad.ShieldBase + " | Ctr: " + string ad.CounterBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer stats effect

    static member tryMakeFromSelection buying inventory selection =
        match snd selection with
        | Consumable ty ->
            match Map.tryFind ty data.Value.Consumables with
            | Some cd -> ShopConfirm.makeFromConsumableData buying inventory selection cd |> Some
            | None -> None
        | Equipment ty ->
            match ty with
            | WeaponType name ->
                match Map.tryFind name data.Value.Weapons with
                | Some wd -> ShopConfirm.makeFromWeaponData buying inventory selection wd |> Some
                | None -> None
            | ArmorType name ->
                match Map.tryFind name data.Value.Armors with
                | Some ad -> ShopConfirm.makeFromArmorData buying inventory selection ad |> Some
                | None -> None
            | AccessoryType name ->
                match Map.tryFind name data.Value.Accessories with
                | Some ad -> ShopConfirm.makeFromAccessoryData buying inventory selection ad |> Some
                | None -> None
        | KeyItem _ | Stash _ -> None

type [<ReferenceEquality; NoComparison>] Shop =
    { ShopType : ShopType
      ShopState : ShopState
      ShopPage : int
      ShopConfirmOpt : ShopConfirm option }

type [<ReferenceEquality; NoComparison>] FieldTransition =
    { FieldType : FieldType
      FieldIndex : Vector2
      FieldDirection : Direction
      FieldTransitionTime : int64 }

[<RequireQualifiedAccess>]
module Field =

    type [<ReferenceEquality; NoComparison>] Field =
        private
            { FieldType_ : FieldType
              Avatar_ : Avatar
              Legion_ : Legion
              Advents_ : Advent Set
              PropStates_ : Map<int, PropState>
              Inventory_ : Inventory
              Submenu_ : Submenu
              ShopOpt_ : Shop option
              FieldTransitionOpt_ : FieldTransition option
              DialogOpt_ : Dialog option
              BattleOpt_ : Battle option }

        (* Local Properties *)
        member this.FieldType = this.FieldType_
        member this.Avatar = this.Avatar_
        member this.Legion = this.Legion_
        member this.Advents = this.Advents_
        member this.PropStates = this.PropStates_
        member this.Inventory = this.Inventory_
        member this.Submenu = this.Submenu_
        member this.ShopOpt = this.ShopOpt_
        member this.FieldTransitionOpt = this.FieldTransitionOpt_
        member this.DialogOpt = this.DialogOpt_
        member this.BattleOpt = this.BattleOpt_

    let getParty field =
        field.Legion_ |>
        Map.filter (fun _ legionnaire -> Option.isSome legionnaire.PartyIndexOpt) |>
        Map.toSeq |>
        Seq.tryTake 3 |>
        Map.ofSeq

    let updateFieldType updater field =
        { field with FieldType_ = updater field.FieldType_ }

    let updateAvatar updater field =
        { field with Avatar_ = updater field.Avatar_ }

    let updateLegion updater field =
        { field with Legion_ = updater field.Legion_ }

    let updateAdvents updater field =
        { field with Advents_ = updater field.Advents_ }

    let updatePropStates updater field =
        { field with PropStates_ = updater field.PropStates_ }

    let updateInventory updater field =
        { field with Inventory_ = updater field.Inventory_ }

    let updateSubmenu updater field =
        { field with Submenu_ = updater field.Submenu_ }

    let updateShopOpt updater field =
        { field with ShopOpt_ = updater field.ShopOpt_ }

    let updateDialogOpt updater field =
        { field with DialogOpt_ = updater field.DialogOpt_ }

    let updateFieldTransitionOpt updater field =
        { field with FieldTransitionOpt_ = updater field.FieldTransitionOpt_ }

    let updateBattleOpt updater field =
        { field with BattleOpt_ = updater field.BattleOpt_ }

    let make fieldType avatar legion advents inventory =
        { FieldType_ = fieldType
          Avatar_ = avatar
          Legion_ = legion
          Advents_ = advents
          PropStates_ = Map.empty
          Inventory_ = inventory
          Submenu_ = { SubmenuState = SubmenuClosed; SubmenuUseOpt = None }
          ShopOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

    let empty =
        { FieldType_ = DebugRoom
          Avatar_ = Avatar.empty
          Legion_ = Map.empty
          Advents_ = Set.empty
          PropStates_ = Map.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          Submenu_ = { SubmenuState = SubmenuClosed; SubmenuUseOpt = None }
          ShopOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

    let initial =
        { FieldType_ = DebugRoom
          Avatar_ = Avatar.empty
          Legion_ = Map.ofList [(0, Legionnaire.finn); (1, Legionnaire.glenn)]
          Advents_ = Set.empty
          PropStates_ = Map.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          Submenu_ = { SubmenuState = SubmenuClosed; SubmenuUseOpt = None }
          ShopOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

type Field = Field.Field