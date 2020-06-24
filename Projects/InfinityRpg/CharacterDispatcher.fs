namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module CharacterDispatcherModule =

    type [<StructuralEquality; NoComparison>] CharacterModel =
        { CharacterActivityState : CharacterActivityState
          CharacterState : CharacterState }

        static member initial =
            { CharacterActivityState = NoActivity
              CharacterState = CharacterState.empty }
    
    type Entity with
        member this.GetCharacterModel = this.GetModel<CharacterModel>
        member this.SetCharacterModel = this.SetModel<CharacterModel>
        member this.CharacterModel = this.Model<CharacterModel> ()
    
    type CharacterDispatcher () =
        inherit EntityDispatcher<CharacterModel, unit, unit> (CharacterModel.initial)

        static member Facets =
            [typeof<CharacterStateFacet>
             typeof<CharacterAnimationFacet>]

        static member Properties =
            [define Entity.PublishChanges true
             define Entity.Omnipresent true]
