﻿namespace MyGame
open System
open Prime
open Nu
open Nu.Declarative

type OptimizedEntityDispatcher () =
    inherit EntityDispatcher ()

    static member Properties =
        [define Entity.Imperative true // makes updates faster by using mutation
         define Entity.IgnoreLayer true // makes actualization faster by not touching the containing layer
         define Entity.Omnipresent true // makes updates faster by not touching the entity tree
         define (Entity.StaticData ()) // makes user-defined properties faster by using local data
            { DesignerType = typeof<Image AssetTag>
              DesignerValue = AssetTag.make<Image> Assets.DefaultPackage "Image4" }]

    override dispatcher.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world + 0.03f) world

    override dispatcher.Actualize (entity, world) =
        if entity.GetVisibleLayered world && entity.GetInView world then
            let position = entity.GetPosition world
            let image = entity.GetStaticData world
            World.enqueueRenderMessage
                (RenderDescriptorsMessage
                    [|LayerableDescriptor
                        { Depth = entity.GetDepthLayered world
                          AssetTag = image
                          PositionY = position.Y
                          LayeredDescriptor =
                          SpriteDescriptor
                            { Position = position
                              Size = entity.GetSize world
                              Rotation = entity.GetRotation world
                              Offset = Vector2.Zero
                              ViewType = entity.GetViewType world
                              InsetOpt = None
                              Image = image
                              Color = Vector4.One }}|])
                world
        else world

type MyGameDispatcher () =
    inherit GameDispatcher<DateTime, unit, unit> (DateTime.UtcNow)

    let Fps = Default.Layer / "Fps"

    override dispatcher.Register (game, world) =
        let world = base.Register (game, world)
        let world = World.createScreen (Some Default.Screen.Name) world |> snd
        let world = World.createLayer (Some Default.Layer.Name) Default.Screen world |> snd
        let world = World.createEntity<TextDispatcher> (Some Fps.Name) DefaultOverlay Default.Layer world |> snd
        let world = Fps.SetPosition (v2 200.0f -250.0f) world
        let indices = // approximately 3000 entities
            seq {
                for i in 0 .. 70 do
                    for j in 0 .. 43 do
                    yield v2 (single i * 12.0f) (single j * 12.0f) }
        let world =
            Seq.fold (fun world position ->
                let (entity, world) = World.createEntity<OptimizedEntityDispatcher> None DefaultOverlay Default.Layer world
                let world = entity.SetPosition (position + v2 -420.0f -265.0f) world
                entity.SetSize (v2One * 8.0f) world)
                world indices
        World.selectScreen Default.Screen world

    override dispatcher.Update (game, world) =
        let startTime = game.GetModel<DateTime> world
        let currentTime = DateTime.UtcNow
        let elapsedTime = currentTime - startTime
        let tickTime = double (World.getTickTime world)
        let frames = tickTime / elapsedTime.TotalSeconds
        let framesStr = "FPS: " + String.Format ("{0:f2}", frames)
        Fps.SetText framesStr world

type MyGamePlugin () =
    inherit NuPlugin ()
    override this.MakeEntityDispatchers () = [OptimizedEntityDispatcher () :> EntityDispatcher]
    override this.MakeGameDispatchers () = [MyGameDispatcher () :> GameDispatcher]
    override this.GetStandAloneGameDispatcherName () = typeof<MyGameDispatcher>.Name
    
module Program =

    // this program exists to take metrics on Nu's performance
    let [<EntryPoint; STAThread>] main _ =
        Nu.init false
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        let tryMakeWorld sdlDeps =
            let plugin = MyGamePlugin ()
            World.tryMake true 1L () plugin sdlDeps
        World.run tryMakeWorld sdlConfig