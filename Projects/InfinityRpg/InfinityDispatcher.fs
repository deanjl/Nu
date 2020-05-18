﻿namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

[<AutoOpen>]
module InfinityDispatcherModule =

    type InfinityCommand =
        | PlayTitleSong
        | FadeSong
        | ShowTitle
        | ShowCredits
        | ShowGameplay of bool
        | ExitGame

    type InfinityDispatcher () =
        inherit GameDispatcher<unit, unit, InfinityCommand> (())

        override this.Register (game, world) =

            // just pre-load all assets in the application for simplicity
            let world = World.hintRenderPackageUse Assets.GuiPackageName world
            let world = World.hintAudioPackageUse Assets.GuiPackageName world
            let world = World.hintRenderPackageUse Assets.GameplayPackageName world
            let world = World.hintAudioPackageUse Assets.GameplayPackageName world

            // get based
            let world = base.Register (game, world)

            // do not persist the hud when saving gameplay
            Simulants.Hud.SetPersistent false world

        override this.Channel (_, _) =
            [Simulants.Title.IncomingStartEvent => [cmd PlayTitleSong]
             Simulants.Title.OutgoingStartEvent => [cmd FadeSong]
             Simulants.TitleCredits.ClickEvent => [cmd ShowCredits]
             Simulants.TitleNewGame.ClickEvent => [cmd (ShowGameplay false)]
             Simulants.TitleLoadGame.ClickEvent => [cmd (ShowGameplay true)]
             Simulants.TitleExit.ClickEvent => [cmd ExitGame]
             Simulants.CreditsBack.ClickEvent => [cmd ShowTitle]
             Simulants.Gameplay.OutgoingStartEvent => [cmd FadeSong]
             Simulants.HudBack.ClickEvent => [cmd ShowTitle]]

        override this.Command (_, command, _, world) =
            let world =
                match command with
                | PlayTitleSong -> World.playSong 0 1.0f Assets.ButterflyGirlSong world
                | FadeSong -> World.fadeOutSong Constants.Audio.DefaultFadeOutMs world
                | ShowTitle -> World.transitionScreen Simulants.Title world
                | ShowCredits -> World.transitionScreen Simulants.Credits world
                | ShowGameplay load -> world |> Simulants.Gameplay.SetShallLoadGame load |> World.transitionScreen Simulants.Gameplay
                | ExitGame -> World.exit world
            just world

        override this.Content (_, _) =
            [Content.screen Simulants.Splash.Name (Splash (Constants.InfinityRpg.DissolveDescriptor, Constants.InfinityRpg.SplashData, Simulants.Title)) [] []
             Content.screenFromLayerFile Simulants.Title.Name (Dissolve (Constants.InfinityRpg.DissolveDescriptor, None)) Assets.TitleLayerFilePath
             Content.screenFromLayerFile Simulants.Credits.Name (Dissolve (Constants.InfinityRpg.DissolveDescriptor, None)) Assets.CreditsLayerFilePath
             Content.screenFromLayerFile<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.InfinityRpg.DissolveDescriptor, None)) Assets.HudLayerFilePath]