﻿namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type PickupType =
    | Health

type [<StructuralEquality; NoComparison>] Pickup =
    { PickupType : PickupType
      PickupSheet : Image AssetTag
      PickupSheetPositionM : Vector2i
      Position : Vector2 }

    static member initial =
        { PickupType = Health
          PickupSheet = Assets.PickupSheetImage
          PickupSheetPositionM = Vector2i.Zero
          Position = Vector2.Zero }

    static member makeHealth positionM =
        { Pickup.initial with Position = vmtovf positionM }