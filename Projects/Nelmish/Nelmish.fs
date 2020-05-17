﻿namespace Nelmish
open Prime
open Nu
open Nu.Declarative

// this is our Elm-style model type
type Model =
    int

// this is our Elm-style message type
type Message =
    | Decrement
    | Increment
    | Reset

// this is our Elm-style game dispatcher
type NelmishDispatcher () =
    inherit GameDispatcher<Model, Message, unit> (0) // initial model value

    // here we handle the Elm-style messages
    override this.Message (model, message, _, _) =
        match message with
        | Decrement -> just (model - 1)
        | Increment -> just (model + 1)
        | Reset -> just 0

    // here we describe the content of the game including its one screen, one layer, three
    // button entities, and one text control.
    override this.Content (model, _) =
        [Content.screen Simulants.DefaultScreen.Name Vanilla []
            [Content.layer Simulants.DefaultLayer.Name []
                [Content.button "Decrement"
                    [Entity.Text == "-"
                     Entity.Position == v2 -256.0f 64.0f
                     Entity.ClickEvent ==> msg Decrement]
                 Content.button "Increment"
                    [Entity.Text == "+"
                     Entity.Position == v2 0.0f 64.0f
                     Entity.ClickEvent ==> msg Increment]
                 Content.text "Counter"
                    [Entity.Text <== model --> scstring
                     Entity.Position == v2 -128.0f -32.0f]
                 Content.entityIf model isNonZero $ fun _ _ ->
                    Content.button "Reset"
                        [Entity.Text == "Reset"
                         Entity.Position == v2 -128.0f -128.0f
                         Entity.ClickEvent ==> msg Reset]]]]