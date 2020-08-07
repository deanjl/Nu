namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type [<CustomEquality; NoComparison>] NavigationNode =
    { PositionM : Vector2i
      mutable Neighbors : NavigationNode list } // OPTIMIZATION: has to be mutable to be efficiently populated.

    interface NavigationNode IHasNeighbors with
        member this.Neighbors = this.Neighbors :> _ seq

    interface NavigationNode IEquatable with
        member this.Equals that =
            this.PositionM = that.PositionM

    override this.Equals that =
        match that with
        | :? NavigationNode as that -> this.PositionM = that.PositionM
        | _ -> false

    override this.GetHashCode () =
        this.PositionM.GetHashCode ()

type WalkState =
    | WalkFinished
    | WalkContinuing

type WalkDescriptor =
    { WalkDirection : Direction
      WalkOriginM : Vector2i }

    member this.NextPositionM =
        this.WalkOriginM + dtovm this.WalkDirection

type [<StructuralEquality; NoComparison>] NavigationDescriptor =
    { WalkDescriptor : WalkDescriptor
      NavigationPathOpt : NavigationNode list option
      LastWalkOriginM : Vector2i }

    member this.NextPositionM =
        this.WalkDescriptor.NextPositionM 

    member this.NextPositionI =
        this.NextPositionM |> vmtovi

    member this.NextPosition =
        this.NextPositionI |> vitovf

type [<StructuralEquality; NoComparison>] ActionDescriptor =
    { ActionTicks : int64 // an arbitrary number to show a hacky action animation
      ActionTargetIndexOpt : int option // None is Player; targetless capacity suspended until proper character indexing is introduced
      ActionDataName : string }

    member this.ComputeActionDirection currentPosition targetPositionM =
        targetPositionM - vftovm currentPosition |> vmtod

type [<StructuralEquality; NoComparison>] CharacterActivityState =
    | Action of ActionDescriptor
    | Navigation of NavigationDescriptor
    | NoActivity

    member this.IsActing =
        match this with
        | Action _ -> true
        | Navigation _ | NoActivity -> false

    member this.IsNotActing =
        not this.IsActing

    member this.IsNavigating =
        match this with
        | Action _ | NoActivity -> false
        | Navigation _ -> true

    member this.IsNotNavigating =
        not this.IsNavigating

    member this.IsNavigatingPath =
        match this with
        | Navigation navigationDescriptor -> Option.isSome navigationDescriptor.NavigationPathOpt
        | Action _ | NoActivity -> false

type [<StructuralEquality; NoComparison>] Turn =
    | ActionTurn of ActionDescriptor
    | NavigationTurn of NavigationDescriptor
    | CancelTurn
    | NoTurn

    member this.IsAction =
        match this with
        | ActionTurn _ -> true
        | _ -> false

    static member makeAttack indexOpt =
        ActionTurn
            { ActionTicks = 0L
              ActionTargetIndexOpt = indexOpt
              ActionDataName = Constants.InfinityRpg.AttackName }