﻿namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module InfinityDispatcher =

    type [<StructuralEquality; NoComparison>] Infinity =
        { Gameplay : Gameplay }
    
    type InfinityCommand =
        | ShowTitle
        | ShowCredits
        | ShowGameplay
        | SetShallLoadGame of bool
        | ExitGame

    type Game with

        member this.GetInfinity = this.GetModel<Infinity>
        member this.SetInfinity = this.SetModel<Infinity>
        member this.Infinity = this.Model<Infinity> ()
    
    type InfinityDispatcher () =
        inherit GameDispatcher<Infinity, unit, InfinityCommand> ({ Gameplay = Gameplay.initial })

        override this.Register (game, world) =

            // just pre-load all assets in the application for simplicity
            let world = World.hintRenderPackageUse Assets.GuiPackageName world
            let world = World.hintAudioPackageUse Assets.GuiPackageName world
            let world = World.hintRenderPackageUse Assets.GameplayPackageName world
            let world = World.hintAudioPackageUse Assets.GameplayPackageName world

            // get based
            base.Register (game, world)

        override this.Channel (_, _) =
            [Simulants.TitleCredits.ClickEvent => cmd ShowCredits
             Simulants.TitleNewGame.ClickEvent => cmd (SetShallLoadGame false)
             Simulants.TitleLoadGame.ClickEvent => cmd (SetShallLoadGame true)
             Simulants.TitleExit.ClickEvent => cmd ExitGame
             Simulants.CreditsBack.ClickEvent => cmd ShowTitle
             Simulants.HudBack.ClickEvent => cmd ShowTitle]

        override this.Command (_, command, _, world) =
            match command with
            | ShowTitle -> World.transitionScreen Simulants.Title world |> just
            | ShowCredits -> World.transitionScreen Simulants.Credits world |> just
            | ShowGameplay -> World.transitionScreen Simulants.Gameplay world |> just
            | SetShallLoadGame shallLoadGame -> Simulants.Gameplay.Gameplay.Update (fun infinity -> { infinity with ShallLoadGame = shallLoadGame }) world |> withCmd ShowGameplay
            | ExitGame -> World.exit world |> just

        override this.Content (infinity, _) =
            [Content.screen Simulants.Splash.Name (Splash (Constants.InfinityRpg.DissolveDescriptor, Constants.InfinityRpg.SplashData, Simulants.Title)) [] []
             Content.screenFromLayerFile Simulants.Title.Name (Dissolve (Constants.InfinityRpg.DissolveDescriptor, Some Assets.ButterflyGirlSong)) Assets.TitleLayerFilePath
             Content.screenFromLayerFile Simulants.Credits.Name (Dissolve (Constants.InfinityRpg.DissolveDescriptor, Some Assets.ButterflyGirlSong)) Assets.CreditsLayerFilePath
             Content.screen<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.InfinityRpg.DissolveDescriptor, Some Assets.HerosVengeanceSong))
                 [Screen.Gameplay <== infinity --> fun infinity -> infinity.Gameplay] []]