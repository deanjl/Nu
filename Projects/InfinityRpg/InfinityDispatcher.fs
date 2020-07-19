namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module InfinityDispatcherModule =

    type [<StructuralEquality; NoComparison>] InfinityModel =
        { GameplayModel : GameplayModel}
    
    type InfinityMessage =
        | SetLoad of bool
    
    type InfinityCommand =
        | ShowTitle
        | ShowCredits
        | ShowGameplay
        | ExitGame

    type Game with

        member this.GetInfinityModel = this.GetModel<InfinityModel>
        member this.SetInfinityModel = this.SetModel<InfinityModel>
        member this.InfinityModel = this.Model<InfinityModel> ()
    
    type InfinityDispatcher () =
        inherit GameDispatcher<InfinityModel, InfinityMessage, InfinityCommand> ({ GameplayModel = GameplayModel.initial })

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
            [Simulants.TitleCredits.ClickEvent => cmd ShowCredits
             Simulants.TitleNewGame.ClickEvent => msg (SetLoad false)
             Simulants.TitleLoadGame.ClickEvent => msg (SetLoad true)
             Simulants.TitleExit.ClickEvent => cmd ExitGame
             Simulants.CreditsBack.ClickEvent => cmd ShowTitle
             Simulants.HudBack.ClickEvent => cmd ShowTitle]

        override this.Message (model, message, _, _) =
            match message with
            | SetLoad load ->
                let gameplayModel = { model.GameplayModel with ShallLoadGame = load }
                let model = { model with GameplayModel = gameplayModel }
                withCmd model ShowGameplay
        
        override this.Command (_, command, _, world) =
            let world =
                match command with
                | ShowTitle -> World.transitionScreen Simulants.Title world
                | ShowCredits -> World.transitionScreen Simulants.Credits world
                | ShowGameplay -> World.transitionScreen Simulants.Gameplay world
                | ExitGame -> World.exit world
            just world

        override this.Content (model, _) =
            [Content.screen Simulants.Splash.Name (Splash (Constants.InfinityRpg.DissolveDescriptor, Constants.InfinityRpg.SplashData, Simulants.Title)) [] []
             Content.screenFromLayerFile Simulants.Title.Name (Dissolve (Constants.InfinityRpg.DissolveDescriptor, Some Assets.ButterflyGirlSong)) Assets.TitleLayerFilePath
             Content.screenFromLayerFile Simulants.Credits.Name (Dissolve (Constants.InfinityRpg.DissolveDescriptor, Some Assets.ButterflyGirlSong)) Assets.CreditsLayerFilePath
             Content.screen<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.InfinityRpg.DissolveDescriptor, Some Assets.HerosVengeanceSong))
                 [Screen.GameplayModel <== model --> fun model -> model.GameplayModel]
                 [Content.layerFromFile Simulants.Hud.Name Assets.HudLayerFilePath]]