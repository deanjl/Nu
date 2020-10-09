namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module PickupDispatcherModule =

    type PickupType =
        | Health
    
    type [<StructuralEquality; NoComparison>] PickupModel =
        { PickupType : PickupType }

        static member health =
            { PickupType = Health }

    type Entity with
        member this.GetPickupModel = this.GetModel<PickupModel>
        member this.SetPickupModel = this.SetModel<PickupModel>
        member this.PickupModel = this.Model<PickupModel> ()

    type PickupDispatcher () =
        inherit EntityDispatcher<PickupModel, unit, unit> (PickupModel.health)