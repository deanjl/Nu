﻿namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type Legionnaire =
    { LegionIndex : int // key
      PartyIndexOpt : int option
      CharacterType : CharacterType
      ExpPoints : int
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Accessories : AccessoryType list }

    static member finn =
        { LegionIndex = 0
          PartyIndexOpt = Some 0
          CharacterType = Ally Finn
          ExpPoints = 15
          WeaponOpt = None
          ArmorOpt = None
          Accessories = [] }

    static member glenn =
        { LegionIndex = 1
          PartyIndexOpt = Some 1
          CharacterType = Ally Glenn
          ExpPoints = 15
          WeaponOpt = None
          ArmorOpt = None
          Accessories = [] }

type [<StructuralEquality; NoComparison>] DialogForm =
    | DialogThin
    | DialogMedium
    | DialogLarge

type [<StructuralEquality; NoComparison>] DialogModel =
    { DialogForm : DialogForm
      DialogText : Dialog
      DialogProgress : int }

[<RequireQualifiedAccess>]
module FieldModel =

    type [<CustomEquality; NoComparison>] FieldModel =
        private
            { Dirty_ : Guid
              FieldType_ : FieldType
              Avatar_ : AvatarModel
              Legion_ : Map<int, Legionnaire>
              Advents_ : Advent Set
              Inventory_ : Inventory
              DialogOpt_ : DialogModel option
              BattleOpt_ : BattleModel option}

        (* Local Properties *)
        member this.FieldType = this.FieldType_
        member this.Avatar = this.Avatar_
        member this.Legion = this.Legion_
        member this.Advents = this.Advents_
        member this.Inventory = this.Inventory_
        member this.DialogOpt = this.DialogOpt_
        member this.BattleOpt = this.BattleOpt_

        (* Equals *)
        override this.GetHashCode () = hash this.Dirty_
        override this.Equals thatObj = match thatObj with :? FieldModel as that -> this.Dirty_ = that.Dirty_ | _ -> false

    let getParty fieldModel =
        fieldModel.Legion_ |>
        Map.filter (fun _ legionnaire -> Option.isSome legionnaire.PartyIndexOpt) |>
        Map.toSeq |>
        Seq.tryTake 3 |>
        Map.ofSeq

    let updateAvatar updater fieldModel =
        { fieldModel with Dirty_ = Gen.id; Avatar_ = updater fieldModel.Avatar_ }

    let updateAdvents updater fieldModel =
        { fieldModel with Dirty_ = Gen.id; Advents_ = updater fieldModel.Advents_ }

    let updateInventory updater fieldModel =
        { fieldModel with Dirty_ = Gen.id; Inventory_ = updater fieldModel.Inventory_ }

    let updateDialogOpt updater fieldModel =
        { fieldModel with Dirty_ = Gen.id; DialogOpt_ = updater fieldModel.DialogOpt_ }

    let updateBattleOpt updater fieldModel =
        { fieldModel with Dirty_ = Gen.id; BattleOpt_ = updater fieldModel.BattleOpt_ }

    let make fieldType avatarModel legion advents inventory =
        { Dirty_ = Gen.id
          FieldType_ = fieldType
          Avatar_ = avatarModel
          Legion_ = legion
          Advents_ = advents
          Inventory_ = inventory
          DialogOpt_ = None
          BattleOpt_ = None }

    let initial =
        { Dirty_ = Gen.id
          FieldType_ = DebugField
          Avatar_ = AvatarModel.empty
          Legion_ = Map.ofList [(0, Legionnaire.finn); (1, Legionnaire.glenn)]
          Advents_ = Set.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          DialogOpt_ = None
          BattleOpt_ = None }

type FieldModel = FieldModel.FieldModel