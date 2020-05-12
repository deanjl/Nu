﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Nito.Collections
open TiledSharp
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module DeclarativeOperators2 =

    type World with

        static member internal attachModel modelValue modelName (simulant : Simulant) world =
            match World.tryGetProperty modelName simulant world with
            | Some property ->
                let model = (property.PropertyValue :?> DesignerProperty).DesignerValue :?> 'model
                (model, world)
            | None ->
                let property = { DesignerType = typeof<'model>; DesignerValue = modelValue }
                let property = { PropertyType = typeof<DesignerProperty>; PropertyValue = property }
                let world = World.attachProperty modelName true false property simulant world
                (modelValue, world)

        static member internal actualizeViews views world =
            Seq.fold (fun world view ->
                match view with
                | Render descriptor -> World.enqueueRenderMessage (RenderDescriptorsMessage [|descriptor|]) world
                | PlaySound (volume, assetTag) -> World.playSound volume assetTag world
                | PlaySong (fade, volume, assetTag) -> World.playSong fade volume assetTag world
                | FadeOutSong fade -> World.fadeOutSong fade world
                | StopSong -> World.stopSong world
                | Tag _ -> world)
                world views

[<AutoOpen>]
module FacetModule =

    type World with

        static member internal trySignalEntityFacet (signalObj : obj) facetName (entity : Entity) world =
            let facets = entity.GetFacets world
            match Array.tryFind (fun facet -> getTypeName facet = facetName) facets with
            | Some (:? Facet<'model, 'message, 'command> as facet) ->
                match signalObj with
                | :? Signal<'message, 'command> as signal ->
                    Signal.processSignal facet.Message facet.Command (entity.FacetModel<'model> facet.ModelName) signal entity world
                | _ -> Log.info "Incorrect signal type returned from event binding."; world
            | _ -> Log.info "Failed to send signal to entity."; world

        static member internal signalEntityFacet<'model, 'message, 'command when 'model : equality> signal facetName (entity : Entity) world =
            let facets = entity.GetFacets world
            match Array.tryFind (fun facet -> getTypeName facet = facetName) facets with
            | Some (:? Facet<'model, 'message, 'command> as facet) ->
                Signal.processSignal facet.Message facet.Command (entity.FacetModel<'model> facet.ModelName) signal entity world
            | _ -> Log.info "Failed to send signal to entity."; world

    and Entity with
    
        member this.GetFacetModel<'model when 'model : equality> modelName world =
            let property = this.Get<DesignerProperty> modelName world
            property.DesignerValue :?> 'model

        member this.SetFacetModel<'model when 'model : equality> modelName (value : 'model) world =
            let model = this.Get<DesignerProperty> modelName world
            if this.GetImperative world
            then model.DesignerValue <- value; world
            else this.Set<DesignerProperty> modelName { model with DesignerValue = value } world

        member this.UpdateFacetModel<'model when 'model : equality> modelName updater world =
            this.SetFacetModel<'model> modelName (updater this.GetFacetModel<'model> modelName world) world

        member this.FacetModel<'model when 'model : equality> modelName =
            lens<'model> modelName (this.GetFacetModel<'model> modelName) (this.SetFacetModel<'model> modelName) this

        member this.TrySignalEntityFacet<'model, 'message, 'command when 'model : equality> signal facetName world =
            World.trySignalEntityFacet signal facetName this world

        member this.SignalEntityFacet<'model, 'message, 'command when 'model : equality> signal facetName world =
            World.signalEntityFacet<'model, 'message, 'command> signal facetName this world

    and [<AbstractClass>] Facet<'model, 'message, 'command when 'model : equality> (initial : 'model) =
        inherit Facet ()

        let mutable modelNameOpt =
            Unchecked.defaultof<string>

        member this.ModelName =
            if isNull modelNameOpt then modelNameOpt <- getTypeName this + "Model"
            modelNameOpt
            
        member this.GetModel (entity : Entity) world : 'model =
            entity.GetFacetModel<'model> this.ModelName world

        member this.SetModel (model : 'model) (entity : Entity) world =
            entity.SetFacetModel<'model> this.ModelName model world

        member this.Model (entity : Entity) =
            lens this.ModelName (this.GetModel entity) (flip this.SetModel entity) entity

        override this.Register (entity, world) =
            let (model, world) = World.attachModel initial this.ModelName entity world
            let channels = this.Channel (model, entity, world)
            let world = Signal.processChannels this.Message this.Command (this.Model entity) channels entity world
            let content = this.Content (this.Model entity, entity, world)
            let world =
                List.fold (fun world content ->
                    World.expandEntityContent None content (FacetOrigin (entity, getTypeName this)) entity.Parent world)
                    world content
            let initializers = this.Initializers (this.Model entity, entity, world)
            List.fold (fun world initializer ->
                match initializer with
                | PropertyDefinition def ->
                    let propertyName = def.PropertyName
                    let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                    let nonPersistent = Reflection.isPropertyNonPersistentByName propertyName
                    let property = { PropertyType = def.PropertyType; PropertyValue = PropertyExpr.eval def.PropertyExpr world }
                    World.setProperty def.PropertyName alwaysPublish nonPersistent property entity world
                | EventHandlerDefinition (handler, partialAddress) ->
                    let eventAddress = partialAddress --> entity
                    World.monitor (fun (evt : Event) world ->
                        WorldModule.trySignalFacet (handler evt) (getTypeName this) entity world)
                        eventAddress (entity :> Simulant) world
                | BindDefinition (left, right, breaking) ->
                    WorldModule.bind5 entity left right breaking world)
                world initializers

        override this.Actualize (entity, world) =
            let views = this.View (this.GetModel entity world, entity, world)
            World.actualizeViews views world

        override this.TrySignal (signalObj, entity, world) =
            match signalObj with
            | :? Signal<'message, obj> as signal -> entity.SignalEntityFacet<'model, 'message, 'command> (match signal with Message message -> msg message | _ -> failwithumf ()) (getTypeName this) world
            | :? Signal<obj, 'command> as signal -> entity.SignalEntityFacet<'model, 'message, 'command> (match signal with Command command -> cmd command | _ -> failwithumf ()) (getTypeName this) world
            | _ -> Log.info "Incorrect signal type returned from event binding."; world

        abstract member Initializers : Lens<'model, World> * Entity * World -> PropertyInitializer list
        default this.Initializers (_, _, _) = []

        abstract member Channel : 'model * Entity * World -> Channel<'message, 'command, Entity, World> list
        default this.Channel (_, _, _) = []

        abstract member Message : 'model * 'message * Entity * World -> 'model * Signal<'message, 'command> list
        default this.Message (model, _, _, _) = just model

        abstract member Command : 'model * 'command * Entity * World -> World * Signal<'message, 'command> list
        default this.Command (_, _, _, world) = just world

        abstract member Content : Lens<'model, World> * Entity * World -> EntityContent list
        default this.Content (_, _, _) = []

        abstract member View : 'model * Entity * World -> View list
        default this.View (_, _, _) = []

[<AutoOpen>]
module EffectFacetModule =

    type EffectTags =
        Map<string, Effects.Slice>

    type Entity with

        member this.GetSelfDestruct world : bool = this.Get Property? SelfDestruct world
        member this.SetSelfDestruct (value : bool) world = this.SetFast Property? SelfDestruct false false value world
        member this.SelfDestruct = lens Property? SelfDestruct this.GetSelfDestruct this.SetSelfDestruct this
        member this.GetEffects world : Symbol AssetTag list = this.Get Property? Effects world
        member this.SetEffects (value : Symbol AssetTag list) world = this.SetFast Property? Effects true false value world
        member this.Effects = lens Property? Effects this.GetEffects this.SetEffects this
        member this.GetEffectStartTimeOpt world : int64 option = this.Get Property? EffectStartTimeOpt world
        member this.SetEffectStartTimeOpt (value : int64 option) world = this.SetFast Property? EffectStartTimeOpt false false value world
        member this.EffectStartTimeOpt = lens Property? EffectStartTimeOpt this.GetEffectStartTimeOpt this.SetEffectStartTimeOpt this
        member this.GetEffectDefinitions world : Effects.Definitions = this.Get Property? EffectDefinitions world
        member this.SetEffectDefinitions (value : Effects.Definitions) world = this.SetFast Property? EffectDefinitions false false value world
        member this.EffectDefinitions = lens Property? EffectDefinitions this.GetEffectDefinitions this.SetEffectDefinitions this
        member this.GetEffect world : Effect = this.Get Property? Effect world
        member this.SetEffect (value : Effect) world = this.SetFast Property? Effect false false value world
        member this.Effect = lens Property? Effect this.GetEffect this.SetEffect this
        member this.GetEffectOffset world : Vector2 = this.Get Property? EffectOffset world
        member this.SetEffectOffset (value : Vector2) world = this.SetFast Property? EffectOffset false false value world
        member this.EffectOffset = lens Property? EffectOffset this.GetEffectOffset this.SetEffectOffset this
        member this.GetEffectPhysicsShapes world : unit = this.Get Property? EffectPhysicsShapes world // NOTE: the default EffectFacet leaves it up to the Dispatcher to do something with the effect's physics output
        member private this.SetEffectPhysicsShapes (value : unit) world = this.SetFast Property? EffectPhysicsShapes false true value world
        member this.EffectPhysicsShapes = lensReadOnly Property? EffectPhysicsShapes this.GetEffectPhysicsShapes this
        member this.GetEffectTags world : EffectTags = this.Get Property? EffectTags world
        member private this.SetEffectTags (value : EffectTags) world = this.SetFast Property? EffectTags false true value world
        member this.EffectTags = lensReadOnly Property? EffectTags this.GetEffectTags this
        member this.GetEffectHistoryMax world : int = this.Get Property? EffectHistoryMax world
        member this.SetEffectHistoryMax (value : int) world = this.SetFast Property? EffectHistoryMax false false value world
        member this.EffectHistoryMax = lens Property? EffectHistoryMax this.GetEffectHistoryMax this.SetEffectHistoryMax this
        member this.GetEffectHistory world : Effects.Slice Deque = this.Get Property? EffectHistory world
        member private this.SetEffectHistory (value : Effects.Slice Deque) world = this.SetFast Property? EffectHistory false true value world
        member this.EffectHistory = lensReadOnly Property? EffectHistory this.GetEffectHistory this
        
        /// The start time of the effect, or zero if none.
        member this.GetEffectStartTime world =
            match this.GetEffectStartTimeOpt world with
            | Some effectStartTime -> effectStartTime
            | None -> 0L

        /// The time relative to the start of the effect.
        member this.GetEffectTime world =
            let effectStartTime = this.GetEffectStartTime world
            let tickTime = World.getTickTime world
            tickTime - effectStartTime

    type EffectFacet () =
        inherit Facet ()

        static let setEffect effects (entity : Entity) world =
            match effects with
            | [] -> world
            | effectAssetTags ->
                let symbolLoadMetadata = { ImplicitDelimiters = false; StripCsvHeader = false }
                let effectOpts = World.assetTagsToValueOpts<Effect> effectAssetTags symbolLoadMetadata world
                let effects = List.definitize effectOpts
                let effectCombined = EffectSystem.combineEffects effects
                entity.SetEffect effectCombined world

        static let handleEffectsChanged evt world =
            let entity = evt.Subscriber : Entity
            let effectsOpt = entity.GetEffects world
            setEffect effectsOpt entity world

        static let handleAssetsReload evt world =
            let entity = evt.Subscriber : Entity
            let effectsOpt = entity.GetEffects world
            setEffect effectsOpt entity world

        static member Properties =
            [define Entity.PublishChanges true
             define Entity.SelfDestruct false
             define Entity.Effects []
             define Entity.EffectStartTimeOpt None
             define Entity.EffectDefinitions Map.empty
             define Entity.Effect Effect.empty
             define Entity.EffectOffset (Vector2 0.5f)
             define Entity.EffectPhysicsShapes ()
             define Entity.EffectTags Map.empty
             define Entity.EffectHistoryMax Constants.Effects.DefaultEffectHistoryMax
             variable Entity.EffectHistory (fun _ -> Deque<Effects.Slice> (inc Constants.Effects.DefaultEffectHistoryMax))]

        override this.Actualize (entity, world) =

            // evaluate effect if visible
            if entity.GetVisibleLayered world && entity.GetInView world then

                // set up effect system to evaluate effect
                let world = entity.SetEffectTags Map.empty world
                let effect = entity.GetEffect world
                let effectTime = entity.GetEffectTime world
                let effectViewType = entity.GetViewType world
                let effectSlice =
                    { Effects.Position = entity.GetPosition world + Vector2.Multiply (entity.GetSize world, entity.GetEffectOffset world)
                      Effects.Size = entity.GetSize world
                      Effects.Rotation = entity.GetRotation world
                      Effects.Depth = entity.GetDepth world
                      Effects.Offset = Vector2 0.5f
                      Effects.InsetOpt = None
                      Effects.Color = Vector4.One
                      Effects.Glow = Vector4.Zero
                      Effects.Enabled = true
                      Effects.Volume = Constants.Audio.DefaultSoundVolume }
                let effectHistory = entity.GetEffectHistory world
                let effectEnv = entity.GetEffectDefinitions world
                let effectSystem = EffectSystem.make effectViewType effectHistory effectTime effectEnv

                // evaluate effect with effect system
                let (artifacts, _) = EffectSystem.eval effect effectSlice effectSystem

                // actualize effect views
                let world = World.actualizeViews artifacts world

                // store tags
                let tags =
                    artifacts |>
                    Seq.toArray |>
                    Array.map (function Tag (name, value) -> Some (name, value :?> Effects.Slice) | _ -> None) |>
                    Array.definitize |> Map.ofArray
                let world = entity.SetEffectTags tags world

                // update effect history in-place
                effectHistory.AddToFront effectSlice
                if effectHistory.Count > entity.GetEffectHistoryMax world then effectHistory.RemoveFromBack () |> ignore
                world

            // no need to evaluate non-visible effect
            else world

        override this.Update (entity, world) =
            let effect = entity.GetEffect world
            match (entity.GetSelfDestruct world, effect.LifetimeOpt) with
            | (true, Some lifetime) ->
                let effectTime = entity.GetEffectTime world
                if effectTime >= dec lifetime // NOTE: dec keeps effect from actualizing past the last frame when it is created mid-frame
                then World.destroyEntity entity world
                else world
            | (_, _) -> world

        override this.Register (entity, world) =
            let effectStartTime = Option.getOrDefault (World.getTickTime world) (entity.GetEffectStartTimeOpt world)
            let world = entity.SetEffectStartTimeOpt (Some effectStartTime) world
            let world = World.monitor handleEffectsChanged (entity.GetChangeEvent Property? Effects) entity world
            World.monitor handleAssetsReload Events.AssetsReload entity world

[<AutoOpen>]
module ScriptFacetModule =

    type Entity with
    
        member this.GetScriptOpt world : Symbol AssetTag option = this.Get Property? ScriptOpt world
        member this.SetScriptOpt (value : Symbol AssetTag option) world = this.SetFast Property? ScriptOpt true false value world
        member this.ScriptOpt = lens Property? ScriptOpt this.GetScriptOpt this.SetScriptOpt this
        member this.GetScript world : Scripting.Expr array = this.Get Property? Script world
        member this.SetScript (value : Scripting.Expr array) world = this.SetFast Property? Script true false value world
        member this.Script = lens Property? Script this.GetScript this.SetScript this
        member internal this.GetScriptUnsubscriptions world : Unsubscription list = this.Get Property? ScriptUnsubscriptions world
        member internal this.SetScriptUnsubscriptions (value : Unsubscription list) world = this.SetFast Property? ScriptUnsubscriptions false true value world
        member internal this.ScriptUnsubscriptions = lens Property? ScriptUnsubscriptions this.GetScriptUnsubscriptions this.SetScriptUnsubscriptions this
        member this.GetRegisterScript world : Scripting.Expr = this.Get Property? RegisterScript world
        member this.SetRegisterScript (value : Scripting.Expr) world = this.SetFast Property? RegisterScript true false value world
        member this.RegisterScript = lens Property? RegisterScript this.GetRegisterScript this.SetRegisterScript this
        member this.GetUnregisterScript world : Scripting.Expr = this.Get Property? UnregisterScript world
        member this.SetUnregisterScript (value : Scripting.Expr) world = this.SetFast Property? UnregisterScript false false value world
        member this.UnregisterScript = lens Property? UnregisterScript this.GetUnregisterScript this.SetUnregisterScript this
        member this.GetUpdateScript world : Scripting.Expr = this.Get Property? UpdateScript world
        member this.SetUpdateScript (value : Scripting.Expr) world = this.SetFast Property? UpdateScript false false value world
        member this.UpdateScript = lens Property? UpdateScript this.GetUpdateScript this.SetUpdateScript this
        member this.GetPostUpdateScript world : Scripting.Expr = this.Get Property? PostUpdateScript world
        member this.SetPostUpdateScript (value : Scripting.Expr) world = this.SetFast Property? PostUpdateScript false false value world
        member this.PostUpdateScript = lens Property? PostUpdateScript this.GetPostUpdateScript this.SetPostUpdateScript this
        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this

    type ScriptFacet () =
        inherit Facet ()

        static let handleScriptChanged evt world =
            let entity = evt.Subscriber : Entity
            let script = entity.GetScript world
            let scriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
            let world = World.setEntityScriptFrame scriptFrame entity world
            evalManyWithLogging script scriptFrame entity world |> snd'

        static let handleRegisterScriptChanged evt world =
            let entity = evt.Subscriber : Entity
            let world = World.unregisterEntity entity world
            World.registerEntity entity world

        static member Properties =
            [define Entity.PublishChanges true
             define Entity.ScriptOpt None
             define Entity.Script [||]
             define Entity.ScriptUnsubscriptions []
             define Entity.RegisterScript Scripting.Unit
             define Entity.UnregisterScript Scripting.Unit
             define Entity.UpdateScript Scripting.Unit
             define Entity.PostUpdateScript Scripting.Unit]

        override this.Register (entity, world) =
            let world = World.evalWithLogging (entity.GetRegisterScript world) (entity.GetScriptFrame world) entity world |> snd'
            let world = World.monitor handleScriptChanged (entity.GetChangeEvent Property? Script) entity world
            let world = World.monitor handleRegisterScriptChanged (entity.GetChangeEvent Property? RegisterScript) entity world
            world

        override this.Unregister (entity, world) =
            World.evalWithLogging (entity.GetUnregisterScript world) (entity.GetScriptFrame world) entity world |> snd'

        override this.Update (entity, world) =
            World.evalWithLogging (entity.GetUpdateScript world) (entity.GetScriptFrame world) entity world |> snd'

        override this.PostUpdate (entity, world) =
            World.evalWithLogging (entity.GetPostUpdateScript world) (entity.GetScriptFrame world) entity world |> snd'

[<AutoOpen>]
module TextFacetModule =

    type Entity with
    
        member this.GetText world : string = this.Get Property? Text world
        member this.SetText (value : string) world = this.SetFast Property? Text false false value world
        member this.Text = lens Property? Text this.GetText this.SetText this
        member this.GetFont world : Font AssetTag = this.Get Property? Font world
        member this.SetFont (value : Font AssetTag) world = this.SetFast Property? Font false false value world
        member this.Font = lens Property? Font this.GetFont this.SetFont this
        member this.GetMargins world : Vector2 = this.Get Property? Margins world
        member this.SetMargins (value : Vector2) world = this.SetFast Property? Margins false false value world
        member this.Margins = lens Property? Margins this.GetMargins this.SetMargins this
        member this.GetJustification world : Justification = this.Get Property? Justification world
        member this.SetJustification (value : Justification) world = this.SetFast Property? Justification false false value world
        member this.Justification = lens Property? Justification this.GetJustification this.SetJustification this
        member this.GetColor world : Vector4 = this.Get Property? Color world
        member this.SetColor (value : Vector4) world = this.SetFast Property? Color false false value world
        member this.Color = lens Property? Color this.GetColor this.SetColor this

    type TextFacet () =
        inherit Facet ()

        static member Properties =
            [define Entity.Text ""
             define Entity.Font (AssetTag.make<Font> Assets.DefaultPackageName Assets.DefaultFontName)
             define Entity.Margins Vector2.Zero
             define Entity.Justification (Justified (JustifyCenter, JustifyMiddle))
             define Entity.Color (Vector4 (0.0f, 0.0f, 0.0f, 1.0f))]

        override this.Actualize (text, world) =
            let textStr = text.GetText world
            if text.GetVisibleLayered world && not (String.IsNullOrWhiteSpace textStr) then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = text.GetDepth world + 0.5f
                              AssetTag = text.GetFont world
                              PositionY = (text.GetPosition world).Y
                              LayeredDescriptor =
                                TextDescriptor
                                    { Text = textStr
                                      Position = text.GetPosition world + text.GetMargins world
                                      Size = text.GetSize world - text.GetMargins world * 2.0f
                                      ViewType = Absolute
                                      Font = text.GetFont world
                                      Color = text.GetColor world
                                      Justification = text.GetJustification world }}|])
                    world
            else world

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with
    
        member this.GetBodyType world : BodyType = this.Get Property? BodyType world
        member this.SetBodyType (value : BodyType) world = this.SetFast Property? BodyType false false value world
        member this.BodyType = lens Property? BodyType this.GetBodyType this.SetBodyType this
        member this.GetAwake world : bool = this.Get Property? Awake world
        member this.SetAwake (value : bool) world = this.SetFast Property? Awake false false value world
        member this.Awake = lens Property? Awake this.GetAwake this.SetAwake this
        member this.GetDensity world : single = this.Get Property? Density world
        member this.SetDensity (value : single) world = this.SetFast Property? Density false false value world
        member this.Density = lens Property? Density this.GetDensity this.SetDensity this
        member this.GetFriction world : single = this.Get Property? Friction world
        member this.SetFriction (value : single) world = this.SetFast Property? Friction false false value world
        member this.Friction = lens Property? Friction this.GetFriction this.SetFriction this
        member this.GetRestitution world : single = this.Get Property? Restitution world
        member this.SetRestitution (value : single) world = this.SetFast Property? Restitution false false value world
        member this.Restitution = lens Property? Restitution this.GetRestitution this.SetRestitution this
        member this.GetFixedRotation world : bool = this.Get Property? FixedRotation world
        member this.SetFixedRotation (value : bool) world = this.SetFast Property? FixedRotation false false value world
        member this.FixedRotation = lens Property? FixedRotation this.GetFixedRotation this.SetFixedRotation this
        member this.GetAngularVelocity world : single = this.Get Property? AngularVelocity world
        member this.SetAngularVelocity (value : single) world = this.SetFast Property? AngularVelocity false false value world
        member this.AngularVelocity = lens Property? AngularVelocity this.GetAngularVelocity this.SetAngularVelocity this
        member this.GetAngularDamping world : single = this.Get Property? AngularDamping world
        member this.SetAngularDamping (value : single) world = this.SetFast Property? AngularDamping false false value world
        member this.AngularDamping = lens Property? AngularDamping this.GetAngularDamping this.SetAngularDamping this
        member this.GetLinearVelocity world : Vector2 = this.Get Property? LinearVelocity world
        member this.SetLinearVelocity (value : Vector2) world = this.SetFast Property? LinearVelocity false false value world
        member this.LinearVelocity = lens Property? LinearVelocity this.GetLinearVelocity this.SetLinearVelocity this
        member this.GetLinearDamping world : single = this.Get Property? LinearDamping world
        member this.SetLinearDamping (value : single) world = this.SetFast Property? LinearDamping false false value world
        member this.LinearDamping = lens Property? LinearDamping this.GetLinearDamping this.SetLinearDamping this
        member this.GetGravityScale world : single = this.Get Property? GravityScale world
        member this.SetGravityScale (value : single) world = this.SetFast Property? GravityScale false false value world
        member this.GravityScale = lens Property? GravityScale this.GetGravityScale this.SetGravityScale this
        member this.GetCollisionCategories world : string = this.Get Property? CollisionCategories world
        member this.SetCollisionCategories (value : string) world = this.SetFast Property? CollisionCategories false false value world
        member this.CollisionCategories = lens Property? CollisionCategories this.GetCollisionCategories this.SetCollisionCategories this
        member this.GetCollisionMask world : string = this.Get Property? CollisionMask world
        member this.SetCollisionMask (value : string) world = this.SetFast Property? CollisionMask false false value world
        member this.CollisionMask = lens Property? CollisionMask this.GetCollisionMask this.SetCollisionMask this
        member this.GetIsBullet world : bool = this.Get Property? IsBullet world
        member this.SetIsBullet (value : bool) world = this.SetFast Property? IsBullet false false value world
        member this.IsBullet = lens Property? IsBullet this.GetIsBullet this.SetIsBullet this
        member this.GetIsSensor world : bool = this.Get Property? IsSensor world
        member this.SetIsSensor (value : bool) world = this.SetFast Property? IsSensor false false value world
        member this.IsSensor = lens Property? IsSensor this.GetIsSensor this.SetIsSensor this
        member this.GetPhysicsId world : PhysicsId = this.Get Property? PhysicsId world
        member this.PhysicsId = lensReadOnly Property? PhysicsId this.GetPhysicsId this
        member this.CollisionEvent = Events.Collision --> this

    type RigidBodyFacet () =
        inherit Facet ()

        static let getBodyShape (entity : Entity) world =
            PhysicsEngine.localizeCollisionBody (entity.GetSize world) (entity.GetCollisionBody world)

        static member Properties =
            [define Entity.BodyType Dynamic
             define Entity.Awake true
             define Entity.Density Constants.Physics.NormalDensity
             define Entity.Friction 0.2f
             define Entity.Restitution 0.0f
             define Entity.FixedRotation false
             define Entity.AngularVelocity 0.0f
             define Entity.AngularDamping 0.0f
             define Entity.LinearVelocity Vector2.Zero
             define Entity.LinearDamping 0.0f
             define Entity.GravityScale 1.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.CollisionBody (BodyBox { Extent = Vector2 0.5f; Center = Vector2.Zero })
             define Entity.IsBullet false
             define Entity.IsSensor false
             computed Entity.PhysicsId (fun (entity : Entity) world -> { SourceId = entity.GetId world; BodyId = Guid.Empty }) None]

        override this.RegisterPhysics (entity, world) =
            let bodyProperties = 
                { BodyId = (entity.GetPhysicsId world).BodyId
                  Position = entity.GetPosition world + entity.GetSize world * 0.5f
                  Rotation = entity.GetRotation world
                  Shape = getBodyShape entity world
                  BodyType = entity.GetBodyType world
                  Awake = entity.GetAwake world
                  Enabled = entity.GetEnabled world
                  Density = entity.GetDensity world
                  Friction = entity.GetFriction world
                  Restitution = entity.GetRestitution world
                  FixedRotation = entity.GetFixedRotation world
                  AngularVelocity = entity.GetAngularVelocity world
                  AngularDamping = entity.GetAngularDamping world
                  LinearVelocity = entity.GetLinearVelocity world
                  LinearDamping = entity.GetLinearDamping world
                  GravityScale = entity.GetGravityScale world
                  CollisionCategories = PhysicsEngine.categorizeCollisionMask (entity.GetCollisionCategories world)
                  CollisionMask = PhysicsEngine.categorizeCollisionMask (entity.GetCollisionMask world)
                  IsBullet = entity.GetIsBullet world
                  IsSensor = entity.GetIsSensor world }
            World.createBody entity (entity.GetId world) bodyProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetPhysicsId world) world

[<AutoOpen>]
module TileMapFacetModule =

    type Entity with
    
        member this.GetTileMapAsset world : TileMap AssetTag = this.Get Property? TileMapAsset world
        member this.SetTileMapAsset (value : TileMap AssetTag) world = this.SetFast Property? TileMapAsset false false value world
        member this.TileMapAsset = lens Property? TileMapAsset this.GetTileMapAsset this.SetTileMapAsset this
        member this.GetTileLayerClearance world : single = this.Get Property? TileLayerClearance world
        member this.SetTileLayerClearance (value : single) world = this.SetFast Property? TileLayerClearance false false value world
        member this.TileLayerClearance = lens Property? TileLayerClearance this.GetTileLayerClearance this.SetTileLayerClearance this
        member this.GetParallax world : single = this.Get Property? Parallax world
        member this.SetParallax (value : single) world = this.SetFast Property? Parallax false false value world
        member this.Parallax = lens Property? Parallax this.GetParallax this.SetParallax this

    type TileMapFacet () =
        inherit Facet ()

        let tryMakeTileMapData (tileMapAsset : TileMap AssetTag) world =
            let metadataMap = World.getMetadata world
            match Metadata.tryGetTileMapMetadata tileMapAsset metadataMap with
            | Some (_, _, map) ->
                let mapSize = Vector2i (map.Width, map.Height)
                let tileSize = Vector2i (map.TileWidth, map.TileHeight)
                let tileSizeF = Vector2 (single tileSize.X, single tileSize.Y)
                let tileMapSize = Vector2i (mapSize.X * tileSize.X, mapSize.Y * tileSize.Y)
                let tileMapSizeF = Vector2 (single tileMapSize.X, single tileMapSize.Y)
                let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
                let tileSetSize =
                    let tileSetWidthOpt = tileSet.Image.Width
                    let tileSetHeightOpt = tileSet.Image.Height
                    Vector2i (tileSetWidthOpt.Value / tileSize.X, tileSetHeightOpt.Value / tileSize.Y)
                Some { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileMapSize = tileMapSize; TileMapSizeF = tileMapSizeF; TileSet = tileSet; TileSetSize = tileSetSize }
            | None -> None

        let makeTileData (tm : Entity) tmd (tl : TmxLayer) tileIndex world =
            let mapRun = tmd.MapSize.X
            let tileSetRun = tmd.TileSetSize.X
            let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
            let tile = tl.Tiles.[tileIndex]
            let gid = tile.Gid - tmd.TileSet.FirstGid
            let gidPosition = gid * tmd.TileSize.X
            let gid2 = Vector2i (gid % tileSetRun, gid / tileSetRun)
            let tileMapPosition = tm.GetPosition world
            let tilePosition =
                Vector2i
                    (int tileMapPosition.X + tmd.TileSize.X * i,
                     int tileMapPosition.Y - tmd.TileSize.Y * (j + 1)) // subtraction for right-handedness
            let tileSetTileOpt =
                match tmd.TileSet.Tiles.TryGetValue (tile.Gid - 1) with
                | (true, tile) -> Some tile
                | (false, _) -> None
            { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; TileSetTileOpt = tileSetTileOpt }

        let getTileBodyProperties6 (tm : Entity) tmd tli td ti cexpr world =
            let tileShape = PhysicsEngine.localizeCollisionBody (Vector2 (single tmd.TileSize.X, single tmd.TileSize.Y)) cexpr
            { BodyId = Gen.idFromInts tli ti
              Position =
                Vector2
                    (single (td.TilePosition.X + tmd.TileSize.X / 2),
                     single (td.TilePosition.Y + tmd.TileSize.Y / 2 + tmd.TileMapSize.Y))
              Rotation = tm.GetRotation world
              Shape = tileShape
              BodyType = BodyType.Static
              Awake = false
              Enabled = true
              Density = Constants.Physics.NormalDensity
              Friction = tm.GetFriction world
              Restitution = tm.GetRestitution world
              FixedRotation = true
              AngularVelocity = 0.0f
              AngularDamping = 0.0f
              LinearVelocity = Vector2.Zero
              LinearDamping = 0.0f
              GravityScale = 0.0f
              CollisionCategories = PhysicsEngine.categorizeCollisionMask (tm.GetCollisionCategories world)
              CollisionMask = PhysicsEngine.categorizeCollisionMask (tm.GetCollisionMask world)
              IsBullet = false
              IsSensor = false }

        let getTileBodyProperties tm tmd (tl : TmxLayer) tli ti world =
            let td = makeTileData tm tmd tl ti world
            match td.TileSetTileOpt with
            | Some tileSetTile ->
                match tileSetTile.Properties.TryGetValue Constants.Physics.CollisionProperty with
                | (true, cexpr) ->
                    let tileBody =
                        match cexpr with
                        | "" -> BodyBox { Extent = Vector2 0.5f; Center = Vector2.Zero }
                        | _ -> scvalue<BodyShape> cexpr
                    let tileBodyProperties = getTileBodyProperties6 tm tmd tli td ti tileBody world
                    Some tileBodyProperties
                | (false, _) -> None
            | None -> None

        let getTileLayerBodyPropertyList tileMap tileMapData tileLayerIndex (tileLayer : TmxLayer) world =
            Seq.foldi
                (fun i bodyPropertyList _ ->
                    match getTileBodyProperties tileMap tileMapData tileLayer tileLayerIndex i world with
                    | Some bodyProperties -> bodyProperties :: bodyPropertyList
                    | None -> bodyPropertyList)
                [] tileLayer.Tiles |>
            Seq.toList

        let registerTileLayerPhysics (tileMap : Entity) tileMapData tileLayerIndex world tileLayer =
            let bodyPropertyList = getTileLayerBodyPropertyList tileMap tileMapData tileLayerIndex tileLayer world
            World.createBodies tileMap (tileMap.GetId world) bodyPropertyList world

        let getTileLayerPhysicsIds (tileMap : Entity) tileMapData tileLayer tileLayerIndex world =
            Seq.foldi
                (fun tileIndex physicsIds _ ->
                    let tileData = makeTileData tileMap tileMapData tileLayer tileIndex world
                    match tileData.TileSetTileOpt with
                    | Some tileSetTile ->
                        if tileSetTile.Properties.ContainsKey Constants.Physics.CollisionProperty then
                            let physicsId = { SourceId = tileMap.GetId world; BodyId = Gen.idFromInts tileLayerIndex tileIndex }
                            physicsId :: physicsIds
                        else physicsIds
                    | None -> physicsIds)
                [] tileLayer.Tiles |>
            Seq.toList

        static member Properties =
            [define Entity.Omnipresent true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.TileMapAsset (AssetTag.make<TileMap> Assets.DefaultPackageName Assets.DefaultTileMapName)
             define Entity.TileLayerClearance 2.0f
             define Entity.Parallax 0.0f]

        override this.RegisterPhysics (tileMap, world) =
            let tileMapAsset = tileMap.GetTileMapAsset world
            match tryMakeTileMapData tileMapAsset world with
            | Some tileMapData ->
                Seq.foldi
                    (registerTileLayerPhysics tileMap tileMapData)
                    world
                    tileMapData.Map.Layers
            | None ->
                Log.debug ("Could not make tile map data for '" + scstring tileMapAsset + "'.")
                world

        override this.UnregisterPhysics (tileMap, world) =
            let tileMapAsset = tileMap.GetTileMapAsset world
            match tryMakeTileMapData tileMapAsset world with
            | Some tileMapData ->
                Seq.foldi
                    (fun tileLayerIndex world (tileLayer : TmxLayer) ->
                        let physicsIds = getTileLayerPhysicsIds tileMap tileMapData tileLayer tileLayerIndex world
                        World.destroyBodies physicsIds world)
                    world
                    tileMapData.Map.Layers
            | None ->
                Log.debug ("Could not make tile map data for '" + scstring tileMapAsset + "'.")
                world

        override this.Actualize (tileMap, world) =
            if tileMap.GetVisible world then
                match Metadata.tryGetTileMapMetadata (tileMap.GetTileMapAsset world) (World.getMetadata world) with
                | Some (_, images, map) ->
                    let viewType = tileMap.GetViewType world
                    let layers = List.ofSeq map.Layers
                    let tileSourceSize = Vector2i (map.TileWidth, map.TileHeight)
                    let tileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                    let tileLayerClearance = tileMap.GetTileLayerClearance world
                    List.foldi
                        (fun i world (layer : TmxLayer) ->
                            List.fold
                                (fun world j ->
                                    let yOffset = single (map.Height - j - 1) * tileSize.Y
                                    let position = tileMap.GetPosition world + v2 0.0f yOffset
                                    let depthOffset =
                                        match layer.Properties.TryGetValue Constants.Physics.DepthProperty with
                                        | (true, depth) -> scvalue depth
                                        | (false, _) -> single i * tileLayerClearance
                                    let depth = tileMap.GetDepth world + depthOffset
                                    let parallaxTranslation =
                                        match viewType with
                                        | Absolute -> Vector2.Zero
                                        | Relative -> tileMap.GetParallax world * depth * -World.getEyeCenter world
                                    let parallaxPosition = position + parallaxTranslation
                                    let size = Vector2 (tileSize.X * single map.Width, tileSize.Y)
                                    let image = List.head images // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                    let tiles =
                                        layer.Tiles |>
                                        enumerable<_> |>
                                        Seq.skip (j * map.Width) |>
                                        Seq.take map.Width |>
                                        Seq.toArray
                                    if World.isBoundsInView viewType (Math.makeBounds parallaxPosition size) world then
                                        World.enqueueRenderMessage
                                            (RenderDescriptorsMessage
                                                [|LayerableDescriptor 
                                                    { Depth = depth
                                                      AssetTag = image
                                                      PositionY = parallaxPosition.Y
                                                      LayeredDescriptor =
                                                        TileLayerDescriptor
                                                            { Position = parallaxPosition
                                                              Size = size
                                                              Rotation = tileMap.GetRotation world
                                                              ViewType = viewType
                                                              MapSize = Vector2i (map.Width, map.Height)
                                                              Tiles = tiles
                                                              TileSourceSize = tileSourceSize
                                                              TileSize = tileSize
                                                              TileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                                              TileSetImage = image }}|])
                                            world
                                    else world)
                                world [0 .. dec map.Height])
                        world layers
                | None -> world
            else world

        override this.GetQuickSize (tileMap, world) =
            match Metadata.tryGetTileMapMetadata (tileMap.GetTileMapAsset world) (World.getMetadata world) with
            | Some (_, _, map) -> Vector2 (single (map.Width * map.TileWidth), single (map.Height * map.TileHeight))
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module NodeFacetModule =

    type Entity with
    
        member this.GetParentNodeOpt world : Entity Relation option = this.Get Property? ParentNodeOpt world
        member this.SetParentNodeOpt (value : Entity Relation option) world = this.SetFast Property? ParentNodeOpt true false value world
        member this.ParentNodeOpt = lens Property? ParentNodeOpt this.GetParentNodeOpt this.SetParentNodeOpt this
        member this.GetPositionLocal world : Vector2 = this.Get Property? PositionLocal world
        member this.SetPositionLocal (value : Vector2) world = this.SetFast Property? PositionLocal false false value world
        member this.PositionLocal = lens Property? PositionLocal this.GetPositionLocal this.SetPositionLocal this
        member this.GetCenterLocal world : Vector2 = this.Get Property? CenterLocal world
        member this.SetCenterLocal (value : Vector2) world = this.SetFast Property? CenterLocal false true value world
        member this.CenterLocal = lens Property? CenterLocal this.GetCenterLocal this.SetCenterLocal this
        member this.GetBottomLocal world : Vector2 = this.Get Property? BottomLocal world
        member this.SetBottomLocal (value : Vector2) world = this.SetFast Property? BottomLocal false true value world
        member this.BottomLocal = lens Property? BottomLocal this.GetBottomLocal this.SetBottomLocal this
        member this.GetDepthLocal world : single = this.Get Property? DepthLocal world
        member this.SetDepthLocal (value : single) world = this.SetFast Property? DepthLocal false false value world
        member this.DepthLocal = lens Property? DepthLocal this.GetDepthLocal this.SetDepthLocal this
        member this.GetVisibleLocal world : bool = this.Get Property? VisibleLocal world
        member this.SetVisibleLocal (value : bool) world = this.SetFast Property? VisibleLocal false false value world
        member this.VisibleLocal = lens Property? VisibleLocal this.GetVisibleLocal this.SetVisibleLocal this
        member this.GetEnabledLocal world : bool = this.Get Property? EnabledLocal world
        member this.SetEnabledLocal (value : bool) world = this.SetFast Property? EnabledLocal false false value world
        member this.EnabledLocal = lens Property? EnabledLocal this.GetEnabledLocal this.SetEnabledLocal this
        member private this.GetNodeUnsubscribe world : World -> World = this.Get Property? NodeUnsubscribe world
        member private this.SetNodeUnsubscribe (value : World -> World) world = this.SetFast Property? NodeUnsubscribe false true value world
        member private this.NodeUnsubscribe = lens Property? NodeUnsubscribe this.GetNodeUnsubscribe this.SetNodeUnsubscribe this

        member this.SetParentNodeOptWithAdjustment (value : Entity Relation option) world =
            let world =
                match (this.GetParentNodeOpt world, value) with
                | (Some relationOld, Some relationNew) ->
                    let parentOld = this.Resolve relationOld
                    let parentNew = this.Resolve relationNew
                    if parentOld.GetExists world && parentNew.GetExists world then
                        let position = this.GetPositionLocal world + parentNew.GetPosition world
                        let depth = this.GetDepthLocal world + parentNew.GetDepth world
                        let world = this.SetPosition position world
                        let world = this.SetDepth depth world
                        let world = this.SetVisible (this.GetVisibleLocal world && parentNew.GetVisible world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world && parentNew.GetEnabled world) world
                        world
                    else world
                | (Some relationOld, None) ->
                    let parentOld = this.Resolve relationOld
                    if parentOld.GetExists world then
                        let position = this.GetPositionLocal world + parentOld.GetPosition world
                        let depth = this.GetDepthLocal world + parentOld.GetDepth world
                        let world = this.SetPosition position world
                        let world = this.SetDepth depth world
                        let world = this.SetVisible (this.GetVisibleLocal world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world) world
                        world
                    else world
                | (None, Some relationNew) ->
                    let parentNew = this.Resolve relationNew
                    if parentNew.GetExists world then
                        let position = this.GetPosition world - parentNew.GetPosition world
                        let depth = this.GetDepth world - parentNew.GetDepth world
                        let world = this.SetPositionLocal position world
                        let world = this.SetDepthLocal depth world
                        let world = this.SetVisible (this.GetVisibleLocal world && parentNew.GetVisible world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world && parentNew.GetEnabled world) world
                        world
                    else world
                | (None, None) -> world
            this.SetParentNodeOpt value world

        member this.GetChildNodes world =
            this.GetChildNodes2 [] world

        member this.ParentNodeExists world =
            match this.GetParentNodeOpt world with
            | Some relation -> (this.Resolve relation).GetExists world
            | None -> false

        member private this.GetChildNodes2 nodes world =
            let nodeOpt =
                if this.FacetedAs<NodeFacet> world
                then Option.map this.Resolve (this.GetParentNodeOpt world)
                else None
            match nodeOpt with
            | Some node -> node.GetChildNodes2 (node :: nodes) world
            | None -> nodes

    and NodeFacet () =
        inherit Facet ()

        static let updatePropertyFromLocal3 propertyName (entity : Entity) world =
            match propertyName with
            | "Position" -> entity.SetPosition (entity.GetPositionLocal world) world
            | "Depth" -> entity.SetDepth (entity.GetDepthLocal world) world
            | "Visible" -> entity.SetVisible (entity.GetVisibleLocal world) world
            | "Enabled" -> entity.SetEnabled (entity.GetEnabledLocal world) world
            | _ -> world

        static let updatePropertyFromLocal propertyName (node : Entity) (entity : Entity) world =
            match propertyName with
            | "PositionLocal" -> entity.SetPosition (node.GetPosition world + entity.GetPositionLocal world) world
            | "DepthLocal" -> entity.SetDepth (node.GetDepth world + entity.GetDepthLocal world) world
            | "VisibleLocal" -> entity.SetVisible (node.GetVisible world && entity.GetVisibleLocal world) world
            | "EnabledLocal" -> entity.SetEnabled (node.GetEnabled world && entity.GetEnabledLocal world) world
            | _ -> world

        static let updatePropertyFromNode propertyName (node : Entity) (entity : Entity) world =
            match propertyName with
            | "Position" -> entity.SetPosition (node.GetPosition world + entity.GetPositionLocal world) world
            | "Depth" -> entity.SetDepth (node.GetDepth world + entity.GetDepthLocal world) world
            | "Visible" -> entity.SetVisible (node.GetVisible world && entity.GetVisibleLocal world) world
            | "Enabled" -> entity.SetEnabled (node.GetEnabled world && entity.GetEnabledLocal world) world
            | _ -> world

        static let updateFromNode (node : Entity) (entity : Entity) world =
            let world = updatePropertyFromNode "Position" node entity world
            let world = updatePropertyFromNode "Depth" node entity world
            let world = updatePropertyFromNode "Visible" node entity world
            let world = updatePropertyFromNode "Enabled" node entity world
            world

        static let tryUpdateFromNode (entity : Entity) world =
            match entity.GetParentNodeOpt world with
            | Some nodeRelation ->
                let node = entity.Resolve nodeRelation
                let world = updateFromNode node entity world
                world
            | None -> world

        static let handleLocalPropertyChange evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : ChangeData
            match entity.GetParentNodeOpt world with
            | Some relation ->
                let node = entity.Resolve relation
                if World.getEntityExists node world
                then (Cascade, updatePropertyFromLocal data.Name node entity world)
                else (Cascade, updatePropertyFromLocal3 data.Name entity world)
            | None -> (Cascade, updatePropertyFromLocal3 data.Name entity world)

        static let handleNodePropertyChange evt world =
            let entity = evt.Subscriber : Entity
            let node = evt.Publisher :?> Entity
            let data = evt.Data : ChangeData
            (Cascade, updatePropertyFromNode data.Name node entity world)

        static let trySubscribeToNodePropertyChanges (entity : Entity) world =
            let oldWorld = world
            let world = (entity.GetNodeUnsubscribe world) world
            match entity.GetParentNodeOpt world with
            | Some nodeRelation ->
                let node = entity.Resolve nodeRelation
                if node = entity then
                    Log.trace "Cannot mount entity to itself."
                    World.choose oldWorld
                elif entity.FacetedAs<RigidBodyFacet> world then
                    Log.trace "Cannot mount a rigid body entity onto another entity. Instead, consider using physics constraints."
                    World.choose oldWorld
                else
                    let (unsubscribe, world) = World.monitorPlus handleNodePropertyChange node.Position.ChangeEvent entity world
                    let (unsubscribe2, world) = World.monitorPlus handleNodePropertyChange node.Depth.ChangeEvent entity world
                    let (unsubscribe3, world) = World.monitorPlus handleNodePropertyChange node.Visible.ChangeEvent entity world
                    let (unsubscribe4, world) = World.monitorPlus handleNodePropertyChange node.Enabled.ChangeEvent entity world
                    entity.SetNodeUnsubscribe (unsubscribe4 >> unsubscribe3 >> unsubscribe2 >> unsubscribe) world
            | None -> world

        static let handleNodeChange evt world =
            let entity = evt.Subscriber
            let world = tryUpdateFromNode entity world
            let world = trySubscribeToNodePropertyChanges entity world
            world

        static member Properties =
            [define Entity.ParentNodeOpt None
             define Entity.PositionLocal Vector2.Zero
             define Entity.DepthLocal 0.0f
             define Entity.VisibleLocal true
             define Entity.EnabledLocal true
             define Entity.NodeUnsubscribe (id : World -> World)
             computed Entity.CenterLocal
                (fun (entity : Entity) world -> entity.GetPositionLocal world + entity.GetSize world * 0.5f)
                (Some (fun value (entity : Entity) world -> entity.SetPositionLocal (value - entity.GetSize world * 0.5f) world))
             computed Entity.BottomLocal
                (fun (entity : Entity) world -> entity.GetPositionLocal world + (entity.GetSize world).WithY 0.0f * 0.5f)
                (Some (fun value (entity : Entity) world -> entity.SetPositionLocal (value - (entity.GetSize world).WithY 0.0f * 0.5f) world))]

        override this.Register (entity, world) =
            let world = entity.SetNodeUnsubscribe id world // ensure unsubscribe function reference doesn't get copied in Gaia...
            let world = World.monitor handleNodeChange entity.ParentNodeOpt.ChangeEvent entity world
            let world = World.monitorPlus handleLocalPropertyChange entity.PositionLocal.ChangeEvent entity world |> snd
            let world = World.monitorPlus handleLocalPropertyChange entity.DepthLocal.ChangeEvent entity world |> snd
            let world = World.monitorPlus handleLocalPropertyChange entity.VisibleLocal.ChangeEvent entity world |> snd
            let world = World.monitorPlus handleLocalPropertyChange entity.EnabledLocal.ChangeEvent entity world |> snd
            let world = tryUpdateFromNode entity world
            let world = trySubscribeToNodePropertyChanges entity world
            world

        override this.Unregister (entity, world) =
            (entity.GetNodeUnsubscribe world) world // NOTE: not sure if this is necessary.

[<AutoOpen>]
module StaticSpriteFacetModule =

    type Entity with

        member this.GetStaticImage world : Image AssetTag = this.Get Property? StaticImage world
        member this.SetStaticImage (value : Image AssetTag) world = this.SetFast Property? StaticImage false false value world
        member this.StaticImage = lens Property? StaticImage this.GetStaticImage this.SetStaticImage this
        member this.GetFlip world : Flip = this.Get Property? Flip world
        member this.SetFlip (value : Flip) world = this.SetFast Property? Flip false false value world
        member this.Flip = lens Property? Flip this.GetFlip this.SetFlip this

    type StaticSpriteFacet () =
        inherit Facet ()

        static member Properties =
            [define Entity.StaticImage (AssetTag.make<Image> Assets.DefaultPackageName "Image4")
             define Entity.Flip FlipNone]

        override this.Actualize (entity, world) =
            if entity.GetVisibleLayered world && entity.GetInView world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = entity.GetDepth world
                              AssetTag = entity.GetStaticImage world
                              PositionY = (entity.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = entity.GetPosition world
                                      Size = entity.GetSize world
                                      Rotation = entity.GetRotation world
                                      Offset = Vector2.Zero
                                      ViewType = entity.GetViewType world
                                      InsetOpt = None
                                      Image = entity.GetStaticImage world
                                      Color = Vector4.One
                                      Glow = Vector4.Zero
                                      Flip = entity.GetFlip world }}|])
                    world
            else world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetStaticImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module AnimatedSpriteFacetModule =

    type Entity with
    
        member this.GetCelSize world : Vector2 = this.Get Property? CelSize world
        member this.SetCelSize (value : Vector2) world = this.SetFast Property? CelSize false false value world
        member this.CelSize = lens Property? CelSize this.GetCelSize this.SetCelSize this
        member this.GetCelRun world : int = this.Get Property? CelRun world
        member this.SetCelRun (value : int) world = this.SetFast Property? CelRun false false value world
        member this.CelRun = lens Property? CelRun this.GetCelRun this.SetCelRun this
        member this.GetCelCount world : int = this.Get Property? CelCount world
        member this.SetCelCount (value : int) world = this.SetFast Property? CelCount false false value world
        member this.CelCount = lens Property? CelCount this.GetCelCount this.SetCelCount this
        member this.GetAnimationDelay world : int64 = this.Get Property? AnimationDelay world
        member this.SetAnimationDelay (value : int64) world = this.SetFast Property? AnimationDelay false false value world
        member this.AnimationDelay = lens Property? AnimationDelay this.GetAnimationDelay this.SetAnimationDelay this
        member this.GetAnimationSheet world : Image AssetTag = this.Get Property? AnimationSheet world
        member this.SetAnimationSheet (value : Image AssetTag) world = this.SetFast Property? AnimationSheet false false value world
        member this.AnimationSheet = lens Property? AnimationSheet this.GetAnimationSheet this.SetAnimationSheet this

    type AnimatedSpriteFacet () =
        inherit Facet ()

        static let getSpriteInsetOpt (entity : Entity) world =
            let celCount = entity.GetCelCount world
            let celRun = entity.GetCelRun world
            if celCount <> 0 && celRun <> 0 then
                let cel = int (World.getTickTime world / entity.GetAnimationDelay world) % celCount
                let celSize = entity.GetCelSize world
                let celI = cel % celRun
                let celJ = cel / celRun
                let celX = single celI * celSize.X
                let celY = single celJ * celSize.Y
                let inset = Vector4 (celX, celY, celX + celSize.X, celY + celSize.Y)
                Some inset
            else None

        static member Properties =
            [define Entity.CelCount 16 
             define Entity.CelSize (Vector2 (16.0f, 16.0f))
             define Entity.CelRun 4
             define Entity.AnimationDelay 4L
             define Entity.AnimationSheet (AssetTag.make<Image> Assets.DefaultPackageName "Image7")
             define Entity.Flip FlipNone]

        override this.Actualize (entity, world) =
            if entity.GetVisibleLayered world && entity.GetInView world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = entity.GetDepth world
                              AssetTag = entity.GetAnimationSheet world
                              PositionY = (entity.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = entity.GetPosition world
                                      Size = entity.GetSize world
                                      Rotation = entity.GetRotation world
                                      Offset = Vector2.Zero
                                      ViewType = entity.GetViewType world
                                      InsetOpt = getSpriteInsetOpt entity world
                                      Image = entity.GetAnimationSheet world
                                      Color = Vector4.One
                                      Glow = Vector4.Zero
                                      Flip = entity.GetFlip world }}|])
                    world
            else world

        override this.GetQuickSize (entity, world) =
            entity.GetCelSize world

[<AutoOpen>]
module EntityDispatcherModule =

    type World with

        static member internal signalEntity<'model, 'message, 'command when 'model : equality> signal (entity : Entity) world =
            match entity.GetDispatcher world with
            | :? EntityDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (entity.Model<'model> ()) signal entity world
            | _ ->
                Log.info "Failed to send signal to entity."
                world

    and Entity with

        member this.GetModel<'model when 'model : equality> world =
            let property = this.Get<DesignerProperty> Property? Model world
            property.DesignerValue :?> 'model

        member this.SetModel<'model when 'model : equality> (value : 'model) world =
            let model = this.Get<DesignerProperty> Property? Model world
            this.Set<DesignerProperty> Property? Model { model with DesignerValue = value } world

        member this.Model<'model when 'model : equality> () =
            lens<'model> Property? Model this.GetModel<'model> this.SetModel<'model> this

        member this.UpdateModel<'model when 'model : equality> updater world =
            this.SetModel<'model> (updater (this.GetModel<'model> world)) world

        member this.Signal<'model, 'message, 'command when 'model : equality> signal world =
            World.signalEntity<'model, 'message, 'command> signal this world

    and [<AbstractClass>] EntityDispatcher<'model, 'message, 'command when 'model : equality> (initial : 'model) =
        inherit EntityDispatcher ()

        member this.GetModel (entity : Entity) world : 'model =
            entity.GetModel<'model> world

        member this.SetModel (model : 'model) (entity : Entity) world =
            entity.SetModel<'model> model world

        member this.Model (entity : Entity) =
            lens Property? Model (this.GetModel entity) (flip this.SetModel entity) entity

        override this.Register (entity, world) =
            let (model, world) = World.attachModel initial Property? Model entity world
            let channels = this.Channel (model, entity, world)
            let world = Signal.processChannels this.Message this.Command (this.Model entity) channels entity world
            let content = this.Content (this.Model entity, entity, world)
            let world =
                List.fold (fun world content ->
                    World.expandEntityContent None content (SimulantOrigin entity) entity.Parent world)
                    world content
            let initializers = this.Initializers (this.Model entity, entity, world)
            List.fold (fun world initializer ->
                match initializer with
                | PropertyDefinition def ->
                    let propertyName = def.PropertyName
                    let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                    let nonPersistent = Reflection.isPropertyNonPersistentByName propertyName
                    let property = { PropertyType = def.PropertyType; PropertyValue = PropertyExpr.eval def.PropertyExpr world }
                    World.setProperty def.PropertyName alwaysPublish nonPersistent property entity world
                | EventHandlerDefinition (handler, partialAddress) ->
                    let eventAddress = partialAddress --> entity
                    World.monitor (fun (evt : Event) world ->
                        WorldModule.trySignal (handler evt) entity world)
                        eventAddress (entity :> Simulant) world
                | BindDefinition (left, right, breaking) ->
                    WorldModule.bind5 entity left right breaking world)
                world initializers

        override this.Actualize (entity, world) =
            let views = this.View (this.GetModel entity world, entity, world)
            World.actualizeViews views world

        override this.TrySignalFacet (signalObj : obj, facetName : string, entity : Entity, world : World) : World =
            entity.TrySignalFacet signalObj facetName world

        override this.TrySignal (signalObj, entity, world) =
            match signalObj with
            | :? Signal<'message, obj> as signal -> entity.Signal<'model, 'message, 'command> (match signal with Message message -> msg message | _ -> failwithumf ()) world
            | :? Signal<obj, 'command> as signal -> entity.Signal<'model, 'message, 'command> (match signal with Command command -> cmd command | _ -> failwithumf ()) world
            | _ -> Log.info "Incorrect signal type returned from event binding."; world

        abstract member Initializers : Lens<'model, World> * Entity * World -> PropertyInitializer list
        default this.Initializers (_, _, _) = []

        abstract member Channel : 'model * Entity * World -> Channel<'message, 'command, Entity, World> list
        default this.Channel (_, _, _) = []

        abstract member Message : 'model * 'message * Entity * World -> 'model * Signal<'message, 'command> list
        default this.Message (model, _, _, _) = just model

        abstract member Command : 'model * 'command * Entity * World -> World * Signal<'message, 'command> list
        default this.Command (_, _, _, world) = just world

        abstract member Content : Lens<'model, World> * Entity * World -> EntityContent list
        default this.Content (_, _, _) = []

        abstract member View : 'model * Entity * World -> View list
        default this.View (_, _, _) = []

[<AutoOpen>]
module EffectDispatcherModule =

    type EffectDispatcher () =
        inherit EntityDispatcher ()

        static member Facets =
            [typeof<EffectFacet>]

        static member Properties =
            [define Entity.PublishChanges true
             define Entity.Effect (scvalue<Effect> "[Effect None [] [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]")]

[<AutoOpen>]
module NodeDispatcherModule =

    type NodeDispatcher () =
        inherit EntityDispatcher ()

        static member Facets =
            [typeof<NodeFacet>]

        static member Properties =
            [define Entity.PublishChanges true]

    type [<AbstractClass>] NodeDispatcher<'model, 'message, 'command when 'model : equality> (model) =
        inherit EntityDispatcher<'model, 'message, 'command> (model)

        static member Facets =
            [typeof<NodeFacet>]

        static member Properties =
            [define Entity.PublishChanges true]

[<AutoOpen>]
module GuiDispatcherModule =

    type Entity with
    
        member this.GetDisabledColor world : Vector4 = this.Get Property? DisabledColor world
        member this.SetDisabledColor (value : Vector4) world = this.SetFast Property? DisabledColor false false value world
        member this.DisabledColor = lens Property? DisabledColor this.GetDisabledColor this.SetDisabledColor this
        member this.GetSwallowMouseLeft world : bool = this.Get Property? SwallowMouseLeft world
        member this.SetSwallowMouseLeft (value : bool) world = this.SetFast Property? SwallowMouseLeft false false value world
        member this.SwallowMouseLeft = lens Property? SwallowMouseLeft this.GetSwallowMouseLeft this.SetSwallowMouseLeft this

    type GuiDispatcher () =
        inherit EntityDispatcher ()

        static let handleMouseLeft evt world =
            let gui = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let handling =
                if gui.GetSelected world && gui.GetVisibleLayered world then
                    let mousePositionWorld = World.mouseToWorld (gui.GetViewType world) data.Position world
                    if data.Down &&
                       gui.GetSwallowMouseLeft world &&
                       Math.isPointInBounds mousePositionWorld (gui.GetBounds world) then
                       Resolve
                    else Cascade
                else Cascade
            (handling, world)

        static member Facets =
            [typeof<NodeFacet>]

        static member Properties =
            [define Entity.PublishChanges true
             define Entity.ViewType Absolute
             define Entity.AlwaysUpdate true
             define Entity.DisabledColor (Vector4 0.75f)
             define Entity.SwallowMouseLeft true]

        override this.Register (gui, world) =
            let world = World.monitorPlus handleMouseLeft Events.MouseLeftDown gui world |> snd
            let world = World.monitorPlus handleMouseLeft Events.MouseLeftUp gui world |> snd
            world

    type [<AbstractClass>] GuiDispatcher<'model, 'message, 'command when 'model : equality> (model) =
        inherit EntityDispatcher<'model, 'message, 'command> (model)

        static let handleMouseLeft evt world =
            let gui = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let handling =
                if gui.GetSelected world && gui.GetVisibleLayered world then
                    let mousePositionWorld = World.mouseToWorld (gui.GetViewType world) data.Position world
                    if data.Down &&
                       gui.GetSwallowMouseLeft world &&
                       Math.isPointInBounds mousePositionWorld (gui.GetBounds world) then
                       Resolve
                    else Cascade
                else Cascade
            (handling, world)

        static member Facets =
            [typeof<NodeFacet>]

        static member Properties =
            [define Entity.PublishChanges true
             define Entity.ViewType Absolute
             define Entity.AlwaysUpdate true
             define Entity.DisabledColor (Vector4 0.75f)
             define Entity.SwallowMouseLeft true]

        override this.Register (gui, world) =
            let world = base.Register (gui, world)
            let world = World.monitorPlus handleMouseLeft Events.MouseLeftDown gui world |> snd
            let world = World.monitorPlus handleMouseLeft Events.MouseLeftUp gui world |> snd
            world

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with
    
        member this.GetDown world : bool = this.Get Property? Down world
        member this.SetDown (value : bool) world = this.SetFast Property? Down false false value world
        member this.Down = lens Property? Down this.GetDown this.SetDown this
        member this.GetUpImage world : Image AssetTag = this.Get Property? UpImage world
        member this.SetUpImage (value : Image AssetTag) world = this.SetFast Property? UpImage false false value world
        member this.UpImage = lens Property? UpImage this.GetUpImage this.SetUpImage this
        member this.GetDownImage world : Image AssetTag = this.Get Property? DownImage world
        member this.SetDownImage (value : Image AssetTag) world = this.SetFast Property? DownImage false false value world
        member this.DownImage = lens Property? DownImage this.GetDownImage this.SetDownImage this
        member this.GetClickSoundOpt world : Audio AssetTag option = this.Get Property? ClickSoundOpt world
        member this.SetClickSoundOpt (value : Audio AssetTag option) world = this.SetFast Property? ClickSoundOpt false false value world
        member this.ClickSoundOpt = lens Property? ClickSoundOpt this.GetClickSoundOpt this.SetClickSoundOpt this
        member this.GetClickSoundVolume world : single = this.Get Property? ClickSoundVolume world
        member this.SetClickSoundVolume (value : single) world = this.SetFast Property? ClickSoundVolume false false value world
        member this.ClickSoundVolume = lens Property? ClickSoundVolume this.GetClickSoundVolume this.SetClickSoundVolume this
        member this.UpEvent = Events.Up --> this
        member this.DownEvent = Events.Down --> this
        member this.ClickEvent = Events.Click --> this

    type ButtonDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown evt world =
            let button = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if button.GetSelected world then
                let mousePositionWorld = World.mouseToWorld (button.GetViewType world) data.Position world
                if  button.GetVisibleLayered world &&
                    Math.isPointInBounds mousePositionWorld (button.GetBounds world) then
                    if button.GetEnabled world then
                        let world = button.SetDown true world
                        let eventTrace = EventTrace.record "ButtonDispatcher" "handleMouseLeftDown" EventTrace.empty
                        let world = World.publish () (Events.Down --> button) eventTrace button world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let button = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if button.GetSelected world then
                let wasDown = button.GetDown world
                let world = button.SetDown false world
                let mousePositionWorld = World.mouseToWorld (button.GetViewType world) data.Position world
                if  button.GetVisibleLayered world &&
                    Math.isPointInBounds mousePositionWorld (button.GetBounds world) then
                    if button.GetEnabled world && wasDown then
                        let eventTrace = EventTrace.record4 "ButtonDispatcher" "handleMouseLeftUp" "Up" EventTrace.empty
                        let world = World.publish () (Events.Up --> button) eventTrace button world
                        let eventTrace = EventTrace.record4 "ButtonDispatcher" "handleMouseLeftUp" "Click" EventTrace.empty
                        let world = World.publish () (Events.Click --> button) eventTrace button world
                        let world =
                            match button.GetClickSoundOpt world with
                            | Some clickSound -> World.playSound (button.GetClickSoundVolume world) clickSound world
                            | None -> world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.Down false
             define Entity.UpImage (AssetTag.make<Image> Assets.DefaultPackageName "Image")
             define Entity.DownImage (AssetTag.make<Image> Assets.DefaultPackageName "Image2")
             define Entity.ClickSoundOpt (Some (AssetTag.make<Audio> Assets.DefaultPackageName "Sound"))
             define Entity.ClickSoundVolume Constants.Audio.DefaultSoundVolume]

        override this.Register (button, world) =
            let world = World.monitorPlus handleMouseLeftDown Events.MouseLeftDown button world |> snd
            let world = World.monitorPlus handleMouseLeftUp Events.MouseLeftUp button world |> snd
            world

        override this.Actualize (button, world) =
            if button.GetVisibleLayered world then
                let image = if button.GetDown world then button.GetDownImage world else button.GetUpImage world
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = button.GetDepth world
                              AssetTag = image
                              PositionY = (button.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = button.GetPosition world
                                      Size = button.GetSize world
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = image
                                      Color = if button.GetEnabled world then Vector4.One else button.GetDisabledColor world
                                      Glow = Vector4.Zero
                                      Flip = FlipNone }}|])
                    world
            else world

        override this.GetQuickSize (button, world) =
            match Metadata.tryGetTextureSizeF (button.GetUpImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module LabelDispatcherModule =

    type Entity with
    
        member this.GetLabelImage world : Image AssetTag = this.Get Property? LabelImage world
        member this.SetLabelImage (value : Image AssetTag) world = this.SetFast Property? LabelImage false false value world
        member this.LabelImage = lens Property? LabelImage this.GetLabelImage this.SetLabelImage this

    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.LabelImage (AssetTag.make<Image> Assets.DefaultPackageName "Image3")]

        override this.Actualize (label, world) =
            if label.GetVisibleLayered world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = label.GetDepth world
                              AssetTag = label.GetLabelImage world
                              PositionY = (label.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = label.GetPosition world
                                      Size = label.GetSize world
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = label.GetLabelImage world
                                      Color = if label.GetEnabled world then Vector4.One else label.GetDisabledColor world
                                      Glow = Vector4.Zero
                                      Flip = FlipNone }}|])
                    world
            else world

        override this.GetQuickSize (label, world) =
            match Metadata.tryGetTextureSizeF (label.GetLabelImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module TextDispatcherModule =

    type Entity with
    
        member this.GetBackgroundImage world : Image AssetTag = this.Get Property? BackgroundImage world
        member this.SetBackgroundImage (value : Image AssetTag) world = this.SetFast Property? BackgroundImage false false value world
        member this.BackgroundImage = lens Property? BackgroundImage this.GetBackgroundImage this.SetBackgroundImage this

    type TextDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.BackgroundImage (AssetTag.make<Image> Assets.DefaultPackageName "Image3")]

        override this.Actualize (text, world) =
            if text.GetVisibleLayered world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = text.GetDepth world
                              AssetTag = text.GetBackgroundImage world
                              PositionY = (text.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = text.GetPosition world
                                      Size = text.GetSize world
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = text.GetBackgroundImage world
                                      Color = if text.GetEnabled world then Vector4.One else text.GetDisabledColor world
                                      Glow = Vector4.Zero
                                      Flip = FlipNone }}|])
                    world
            else world

        override this.GetQuickSize (text, world) =
            match Metadata.tryGetTextureSizeF (text.GetBackgroundImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module ToggleDispatcherModule =

    type Entity with
    
        member this.GetOpen world : bool = this.Get Property? Open world
        member this.SetOpen (value : bool) world = this.SetFast Property? Open false false value world
        member this.Open = lens Property? Open this.GetOpen this.SetOpen this
        member this.GetPressed world : bool = this.Get Property? Pressed world
        member this.SetPressed (value : bool) world = this.SetFast Property? Pressed false false value world
        member this.Pressed = lens Property? Pressed this.GetPressed this.SetPressed this
        member this.GetOpenImage world : Image AssetTag = this.Get Property? OpenImage world
        member this.SetOpenImage (value : Image AssetTag) world = this.SetFast Property? OpenImage false false value world
        member this.OpenImage = lens Property? OpenImage this.GetOpenImage this.SetOpenImage this
        member this.GetClosedImage world : Image AssetTag = this.Get Property? ClosedImage world
        member this.SetClosedImage (value : Image AssetTag) world = this.SetFast Property? ClosedImage false false value world
        member this.ClosedImage = lens Property? ClosedImage this.GetClosedImage this.SetClosedImage this
        member this.GetToggleSoundOpt world : Audio AssetTag option = this.Get Property? ToggleSoundOpt world
        member this.SetToggleSoundOpt (value : Audio AssetTag option) world = this.SetFast Property? ToggleSoundOpt false false value world
        member this.ToggleSoundOpt = lens Property? ToggleSoundOpt this.GetToggleSoundOpt this.SetToggleSoundOpt this
        member this.GetToggleSoundVolume world : single = this.Get Property? ToggleSoundVolume world
        member this.SetToggleSoundVolume (value : single) world = this.SetFast Property? ToggleSoundVolume false false value world
        member this.ToggleSoundVolume = lens Property? ToggleSoundVolume this.GetToggleSoundVolume this.SetToggleSoundVolume this
        member this.ToggleEvent = Events.Toggle --> this

    type ToggleDispatcher () =
        inherit GuiDispatcher ()
        
        let handleMouseLeftDown evt world =
            let toggle = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if toggle.GetSelected world then
                let mousePositionWorld = World.mouseToWorld (toggle.GetViewType world) data.Position world
                if  toggle.GetVisibleLayered world &&
                    Math.isPointInBounds mousePositionWorld (toggle.GetBounds world) then
                    if toggle.GetEnabled world then
                        let world = toggle.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let toggle = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if toggle.GetSelected world then
                let wasPressed = toggle.GetPressed world
                let world = toggle.SetPressed false world
                let mousePositionWorld = World.mouseToWorld (toggle.GetViewType world) data.Position world
                if  toggle.GetVisibleLayered world &&
                    Math.isPointInBounds mousePositionWorld (toggle.GetBounds world) then
                    if toggle.GetEnabled world && wasPressed then
                        let world = toggle.SetOpen (not (toggle.GetOpen world)) world
                        let eventAddress = if toggle.GetOpen world then Events.Open else Events.Close
                        let eventTrace = EventTrace.record "ToggleDispatcher" "handleMouseLeftUp" EventTrace.empty
                        let world = World.publish () (eventAddress --> toggle) eventTrace toggle world
                        let eventTrace = EventTrace.record4 "ToggleDispatcher" "handleMouseLeftUp" "Toggle" EventTrace.empty
                        let world = World.publish () (Events.Toggle --> toggle) eventTrace toggle world
                        let world =
                            match toggle.GetToggleSoundOpt world with
                            | Some toggleSound -> World.playSound (toggle.GetToggleSoundVolume world) toggleSound world
                            | None -> world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.Open true
             define Entity.Pressed false
             define Entity.OpenImage (AssetTag.make<Image> Assets.DefaultPackageName "Image")
             define Entity.ClosedImage (AssetTag.make<Image> Assets.DefaultPackageName "Image2")
             define Entity.ToggleSoundOpt (Some (AssetTag.make<Audio> Assets.DefaultPackageName "Sound"))
             define Entity.ToggleSoundVolume Constants.Audio.DefaultSoundVolume]

        override this.Register (toggle, world) =
            let world = World.monitorPlus handleMouseLeftDown Events.MouseLeftDown toggle world |> snd
            let world = World.monitorPlus handleMouseLeftUp Events.MouseLeftUp toggle world |> snd
            world

        override this.Actualize (toggle, world) =
            if toggle.GetVisibleLayered world then
                let image =
                    if toggle.GetOpen world && not (toggle.GetPressed world)
                    then toggle.GetOpenImage world
                    else toggle.GetClosedImage world
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = toggle.GetDepth world
                              AssetTag = image
                              PositionY = (toggle.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = toggle.GetPosition world
                                      Size = toggle.GetSize world
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = image
                                      Color = if toggle.GetEnabled world then Vector4.One else toggle.GetDisabledColor world
                                      Glow = Vector4.Zero
                                      Flip = FlipNone }}|])
                    world
            else world

        override this.GetQuickSize (toggle, world) =
            match Metadata.tryGetTextureSizeF (toggle.GetOpenImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize
            
[<AutoOpen>]
module FpsDispatcherModule =

    type Entity with
    
        member this.GetStartTickTime world : int64 = this.Get Property? StartTickTime world
        member this.SetStartTickTime (value : int64) world = this.SetFast Property? StartTickTime false false value world
        member this.StartTickTime = lens Property? StartTickTime this.GetStartTickTime this.SetStartTickTime this
        member this.GetStartDateTime world : DateTime = this.Get Property? StartDateTime world
        member this.SetStartDateTime (value : DateTime) world = this.SetFast Property? StartDateTime false false value world
        member this.StartDateTime = lens Property? StartDateTime this.GetStartDateTime this.SetStartDateTime this

    type FpsDispatcher () =
        inherit TextDispatcher ()

        static let resetIntermittent (entity : Entity) world =
            let startDateTime = entity.GetStartDateTime world
            let currentDateTime = DateTime.UtcNow
            let elapsedDateTime = currentDateTime - startDateTime
            if elapsedDateTime.TotalSeconds >= 4.0 then
                let world = entity.SetStartTickTime (World.getTickTime world) world
                entity.SetStartDateTime currentDateTime world
            else world

        static member Properties =
            [define Entity.StartTickTime 0L
             define Entity.StartDateTime DateTime.UtcNow]

        override this.Update (entity, world) =
            let world = resetIntermittent entity world
            let startDateTime = entity.GetStartDateTime world
            let currentDateTime = DateTime.UtcNow
            let elapsedDateTime = currentDateTime - startDateTime
            let tickTime = double (World.getTickTime world - entity.GetStartTickTime world)
            let frames = tickTime / elapsedDateTime.TotalSeconds
            if not (Double.IsNaN frames) then 
                let framesStr = "FPS: " + String.Format ("{0:f2}", frames)
                entity.SetText framesStr world
            else world

[<AutoOpen>]
module FeelerDispatcherModule =

    type Entity with
    
        member this.GetTouched world : bool = this.Get Property? Touched world
        member this.SetTouched (value : bool) world = this.SetFast Property? Touched false false value world
        member this.Touched = lens Property? Touched this.GetTouched this.SetTouched this
        member this.TouchEvent = Events.Touch --> this
        member this.UntouchEvent = Events.Untouch --> this

    type FeelerDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown evt world =
            let feeler = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if feeler.GetSelected world then
                let mousePositionWorld = World.mouseToWorld (feeler.GetViewType world) data.Position world
                if  feeler.GetVisibleLayered world &&
                    Math.isPointInBounds mousePositionWorld (feeler.GetBounds world) then
                    if feeler.GetEnabled world then
                        let world = feeler.SetTouched true world
                        let eventTrace = EventTrace.record "FeelerDispatcher" "handleMouseLeftDown" EventTrace.empty
                        let world = World.publish data.Position (Events.Touch --> feeler) eventTrace feeler world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let feeler = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if feeler.GetSelected world && feeler.GetVisibleLayered world then
                if feeler.GetEnabled world then
                    let world = feeler.SetTouched false world
                    let eventTrace = EventTrace.record "FeelerDispatcher" "handleMouseLeftDown" EventTrace.empty
                    let world = World.publish data.Position (Events.Untouch --> feeler) eventTrace feeler world
                    (Resolve, world)
                else (Resolve, world)
            else (Cascade, world)

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.Touched false]

        override this.Register (feeler, world) =
            let world = World.monitorPlus handleMouseLeftDown Events.MouseLeftDown feeler world |> snd
            let world = World.monitorPlus handleMouseLeftUp Events.MouseLeftUp feeler world |> snd
            world

        override this.GetQuickSize (_, _) =
            Vector2 64.0f

[<AutoOpen>]
module FillBarDispatcherModule =

    type Entity with
    
        member this.GetFill world : single = this.Get Property? Fill world
        member this.SetFill (value : single) world = this.SetFast Property? Fill false false value world
        member this.Fill = lens Property? Fill this.GetFill this.SetFill this
        member this.GetFillInset world : single = this.Get Property? FillInset world
        member this.SetFillInset (value : single) world = this.SetFast Property? FillInset false false value world
        member this.FillInset = lens Property? FillInset this.GetFillInset this.SetFillInset this
        member this.GetFillImage world : Image AssetTag = this.Get Property? FillImage world
        member this.SetFillImage (value : Image AssetTag) world = this.SetFast Property? FillImage false false value world
        member this.FillImage = lens Property? FillImage this.GetFillImage this.SetFillImage this
        member this.GetBorderImage world : Image AssetTag = this.Get Property? BorderImage world
        member this.SetBorderImage (value : Image AssetTag) world = this.SetFast Property? BorderImage false false value world
        member this.BorderImage = lens Property? BorderImage this.GetBorderImage this.SetBorderImage this

    type FillBarDispatcher () =
        inherit GuiDispatcher ()
        
        let getFillBarSpriteDims (fillBar : Entity) world =
            let spriteSize = fillBar.GetSize world
            let spriteInset = spriteSize * fillBar.GetFillInset world * 0.5f
            let spritePosition = fillBar.GetPosition world + spriteInset
            let spriteWidth = (spriteSize.X - spriteInset.X * 2.0f) * fillBar.GetFill world
            let spriteHeight = spriteSize.Y - spriteInset.Y * 2.0f
            (spritePosition, Vector2 (spriteWidth, spriteHeight))

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.Fill 0.0f
             define Entity.FillInset 0.0f
             define Entity.FillImage (AssetTag.make<Image> Assets.DefaultPackageName "Image9")
             define Entity.BorderImage (AssetTag.make<Image> Assets.DefaultPackageName "Image10")]

        override this.Actualize (fillBar, world) =
            if fillBar.GetVisibleLayered world then
                let (fillBarSpritePosition, fillBarSpriteSize) = getFillBarSpriteDims fillBar world
                let fillBarColor = if fillBar.GetEnabled world then Vector4.One else fillBar.GetDisabledColor world
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = fillBar.GetDepth world + 0.5f
                              AssetTag = fillBar.GetBorderImage world
                              PositionY = (fillBar.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = fillBar.GetPosition world
                                      Size = fillBar.GetSize world
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = fillBar.GetBorderImage world
                                      Color = fillBarColor
                                      Glow = Vector4.Zero
                                      Flip = FlipNone }}
                          LayerableDescriptor
                            { Depth = fillBar.GetDepth world
                              AssetTag = fillBar.GetFillImage world
                              PositionY = (fillBar.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = fillBarSpritePosition
                                      Size = fillBarSpriteSize
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = fillBar.GetFillImage world
                                      Color = fillBarColor
                                      Glow = Vector4.Zero
                                      Flip = FlipNone }}|])
                    world
            else world

        override this.GetQuickSize (fillBar, world) =
            match Metadata.tryGetTextureSizeF (fillBar.GetBorderImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module BlockDispatcherModule =

    type BlockDispatcher () =
        inherit EntityDispatcher ()

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.BodyType Static
             define Entity.StaticImage (AssetTag.make<Image> Assets.DefaultPackageName "Image4")]

[<AutoOpen>]
module BoxDispatcherModule =

    type BoxDispatcher () =
        inherit EntityDispatcher ()

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.StaticImage (AssetTag.make<Image> Assets.DefaultPackageName "Image4")]

[<AutoOpen>]
module CharacterDispatcherModule =

    type Entity with
        
        member this.GetCharacterIdleImage world = this.Get Property? CharacterIdleImage world
        member this.SetCharacterIdleImage value world = this.SetFast Property? CharacterIdleImage false false value world
        member this.CharacterIdleImage = lens<Image AssetTag> Property? CharacterIdleImage this.GetCharacterIdleImage this.SetCharacterIdleImage this
        member this.GetCharacterJumpImage world = this.Get Property? CharacterJumpImage world
        member this.SetCharacterJumpImage value world = this.SetFast Property? CharacterJumpImage false false value world
        member this.CharacterJumpImage = lens<Image AssetTag> Property? CharacterJumpImage this.GetCharacterJumpImage this.SetCharacterJumpImage this
        member this.GetCharacterWalkSheet world = this.Get Property? CharacterWalkSheet world
        member this.SetCharacterWalkSheet value world = this.SetFast Property? CharacterWalkSheet false false value world
        member this.CharacterWalkSheet = lens<Image AssetTag> Property? CharacterWalkSheet this.GetCharacterWalkSheet this.SetCharacterWalkSheet this
        member this.GetCharacterFacingLeft world = this.Get Property? CharacterFacingLeft world
        member this.SetCharacterFacingLeft value world = this.SetFast Property? CharacterFacingLeft false false value world
        member this.CharacterFacingLeft = lens<bool> Property? CharacterFacingLeft this.GetCharacterFacingLeft this.SetCharacterFacingLeft this
        
    type CharacterDispatcher () =
        inherit EntityDispatcher ()

        static let computeWalkCelInset (celSize : Vector2) (celRun : int) delay time =
            let timeCompressed = time / delay
            let frame = timeCompressed % int64 celRun
            let i = single (frame % 3L)
            let j = single (frame / 3L)
            let offset = v2 (i * celSize.X) (j * celSize.Y) 
            v4 offset.X offset.Y (offset.X + celSize.X) (offset.Y + celSize.Y)

        static member Facets =
            [typeof<RigidBodyFacet>]

        static member Properties =
            [define Entity.AnimationDelay 6L
             define Entity.CelSize (v2 28.0f 28.0f)
             define Entity.CelRun 8
             define Entity.FixedRotation true
             define Entity.GravityScale 3.0f
             define Entity.CollisionBody (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = v2Zero })
             define Entity.CharacterIdleImage (AssetTag.make Assets.DefaultPackageName "CharacterIdle")
             define Entity.CharacterJumpImage (AssetTag.make Assets.DefaultPackageName "CharacterJump")
             define Entity.CharacterWalkSheet (AssetTag.make Assets.DefaultPackageName "CharacterWalk")
             define Entity.CharacterFacingLeft false]

        override this.Update (entity, world) =
            // we have to a bit of hackery to remember whether the character is facing left or right
            // when there is no velocity
            let facingLeft = entity.GetCharacterFacingLeft world
            let velocity = World.getBodyLinearVelocity (entity.GetPhysicsId world) world
            if facingLeft && velocity.X > 1.0f then entity.SetCharacterFacingLeft false world
            elif not facingLeft && velocity.X < -1.0f then entity.SetCharacterFacingLeft true world
            else world

        override this.Actualize (entity, world) =
            if entity.GetVisibleLayered world && entity.GetInView world then
                let time = World.getTickTime world
                let physicsId = entity.GetPhysicsId world
                let facingLeft = entity.GetCharacterFacingLeft world
                let velocity = World.getBodyLinearVelocity physicsId world
                let celSize = entity.GetCelSize world
                let celRun = entity.GetCelRun world
                let animationDelay = entity.GetAnimationDelay world
                let (insetOpt, image) =
                    if not (World.isBodyOnGround physicsId world) then
                        let image = entity.GetCharacterJumpImage world
                        (None, image)
                    elif velocity.X < 5.0f && velocity.X > -5.0f then
                        let image = entity.GetCharacterIdleImage world
                        (None, image)
                    else
                        let image = entity.GetCharacterWalkSheet world
                        (Some (computeWalkCelInset celSize celRun animationDelay time), image)
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = entity.GetDepth world
                              AssetTag = image
                              PositionY = (entity.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = entity.GetPosition world
                                      Size = entity.GetSize world
                                      Rotation = entity.GetRotation world
                                      Offset = v2Zero
                                      ViewType = entity.GetViewType world
                                      InsetOpt = insetOpt
                                      Image = image
                                      Color = Vector4.One
                                      Glow = Vector4.Zero
                                      Flip = if facingLeft then FlipH else FlipNone }}|])
                    world
            else world

[<AutoOpen>]
module TileMapDispatcherModule =

    type TileMapDispatcher () =
        inherit EntityDispatcher ()

        static member Facets =
            [typeof<TileMapFacet>]

        static member Properties =
            [define Entity.Omnipresent true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.TileMapAsset (AssetTag.make<TileMap> Assets.DefaultPackageName Assets.DefaultTileMapName)
             define Entity.Parallax 0.0f]

[<AutoOpen>]
module LayerDispatcherModule =

    type World with

        static member internal signalLayer<'model, 'message, 'command when 'model : equality> signal (layer : Layer) world =
            match layer.GetDispatcher world with
            | :? LayerDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (layer.Model<'model> ()) signal layer world
            | _ ->
                Log.info "Failed to send signal to layer."
                world

    and Layer with
    
        member this.GetModel<'model when 'model : equality> world =
            let property = this.Get<DesignerProperty> Property? Model world
            property.DesignerValue :?> 'model

        member this.SetModel<'model when 'model : equality> (value : 'model) world =
            let model = this.Get<DesignerProperty> Property? Model world
            this.Set<DesignerProperty> Property? Model { model with DesignerValue = value } world

        member this.Model<'model when 'model : equality> () =
            lens<'model> Property? Model this.GetModel<'model> this.SetModel<'model> this

        member this.UpdateModel<'model when 'model : equality> updater world =
            this.SetModel<'model> (updater (this.GetModel<'model> world)) world

        member this.Signal<'model, 'message, 'command when 'model : equality> signal world =
            World.signalLayer<'model, 'message, 'command> signal this world

    and [<AbstractClass>] LayerDispatcher<'model, 'message, 'command when 'model : equality> (initial : 'model) =
        inherit LayerDispatcher ()

        member this.GetModel (layer : Layer) world : 'model =
            layer.GetModel<'model> world

        member this.SetModel (model : 'model) (layer : Layer) world =
            layer.SetModel<'model> model world

        member this.Model (layer : Layer) =
            lens Property? Model (this.GetModel layer) (flip this.SetModel layer) layer

        override this.Register (layer, world) =
            let (model, world) = World.attachModel initial Property? Model layer world
            let channels = this.Channel (model, layer, world)
            let world = Signal.processChannels this.Message this.Command (this.Model layer) channels layer world
            let content = this.Content (this.Model layer, layer, world)
            let world =
                List.fold (fun world content ->
                    World.expandEntityContent None content (SimulantOrigin layer) layer world)
                    world content
            let initializers = this.Initializers (this.Model layer, layer, world)
            List.fold (fun world initializer ->
                match initializer with
                | PropertyDefinition def ->
                    let propertyName = def.PropertyName
                    let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                    let nonPersistent = Reflection.isPropertyNonPersistentByName propertyName
                    let property = { PropertyType = def.PropertyType; PropertyValue = PropertyExpr.eval def.PropertyExpr world }
                    World.setProperty def.PropertyName alwaysPublish nonPersistent property layer world
                | EventHandlerDefinition (handler, partialAddress) ->
                    let eventAddress = partialAddress --> layer
                    World.monitor (fun (evt : Event) world ->
                        WorldModule.trySignal (handler evt) layer world)
                        eventAddress (layer :> Simulant) world
                | BindDefinition (left, right, breaking) ->
                    WorldModule.bind5 layer left right breaking world)
                world initializers

        override this.Actualize (layer, world) =
            let views = this.View (this.GetModel layer world, layer, world)
            World.actualizeViews views world

        override this.TrySignal (signalObj, layer, world) =
            match signalObj with
            | :? Signal<'message, obj> as signal -> layer.Signal<'model, 'message, 'command> (match signal with Message message -> msg message | _ -> failwithumf ()) world
            | :? Signal<obj, 'command> as signal -> layer.Signal<'model, 'message, 'command> (match signal with Command command -> cmd command | _ -> failwithumf ()) world
            | _ -> Log.info "Incorrect signal type returned from event binding."; world

        abstract member Initializers : Lens<'model, World> * Layer * World -> PropertyInitializer list
        default this.Initializers (_, _, _) = []

        abstract member Channel : 'model * Layer * World -> Channel<'message, 'command, Layer, World> list
        default this.Channel (_, _, _) = []

        abstract member Message : 'model * 'message * Layer * World -> 'model * Signal<'message, 'command> list
        default this.Message (model, _, _, _) = just model

        abstract member Command : 'model * 'command * Layer * World -> World * Signal<'message, 'command> list
        default this.Command (_, _, _, world) = just world

        abstract member Content : Lens<'model, World> * Layer * World -> EntityContent list
        default this.Content (_, _, _) = []

        abstract member View : 'model * Layer * World -> View list
        default this.View (_, _, _) = []

[<AutoOpen>]
module ScreenDispatcherModule =

    type World with

        static member internal signalScreen<'model, 'message, 'command when 'model : equality> signal (screen : Screen) world =
            match screen.GetDispatcher world with
            | :? ScreenDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (screen.Model<'model> ()) signal screen world
            | _ ->
                Log.info "Failed to send signal to screen."
                world

    and Screen with
    
        member this.GetModel<'model> world =
            let property = this.Get<DesignerProperty> Property? Model world
            property.DesignerValue :?> 'model

        member this.SetModel<'model when 'model : equality> (value : 'model) world =
            let model = this.Get<DesignerProperty> Property? Model world
            this.Set<DesignerProperty> Property? Model { model with DesignerValue = value } world

        member this.Model<'model when 'model : equality> () =
            lens<'model> Property? Model this.GetModel<'model> this.SetModel<'model> this

        member this.UpdateModel<'model when 'model : equality> updater world =
            this.SetModel<'model> (updater (this.GetModel<'model> world)) world

        member this.Signal<'model, 'message, 'command when 'model : equality> signal world =
            World.signalScreen<'model, 'message, 'command> signal this world

    and [<AbstractClass>] ScreenDispatcher<'model, 'message, 'command when 'model : equality> (initial : 'model) =
        inherit ScreenDispatcher ()

        member this.GetModel (screen : Screen) world : 'model =
            screen.GetModel<'model> world

        member this.SetModel (model : 'model) (screen : Screen) world =
            screen.SetModel<'model> model world

        member this.Model (screen : Screen) =
            lens Property? Model (this.GetModel screen) (flip this.SetModel screen) screen

        override this.Register (screen, world) =
            let (model, world) = World.attachModel initial Property? Model screen world
            let channels = this.Channel (model, screen, world)
            let world = Signal.processChannels this.Message this.Command (this.Model screen) channels screen world
            let content = this.Content (this.Model screen, screen, world)
            let world =
                List.fold (fun world content ->
                    World.expandLayerContent None content (SimulantOrigin screen) screen world)
                    world content
            let initializers = this.Initializers (this.Model screen, screen, world)
            List.fold (fun world initializer ->
                match initializer with
                | PropertyDefinition def ->
                    let propertyName = def.PropertyName
                    let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                    let nonPersistent = Reflection.isPropertyNonPersistentByName propertyName
                    let property = { PropertyType = def.PropertyType; PropertyValue = PropertyExpr.eval def.PropertyExpr world }
                    World.setProperty def.PropertyName alwaysPublish nonPersistent property screen world
                | EventHandlerDefinition (handler, partialAddress) ->
                    let eventAddress = partialAddress --> screen
                    World.monitor (fun (evt : Event) world ->
                        WorldModule.trySignal (handler evt) screen world)
                        eventAddress (screen :> Simulant) world
                | BindDefinition (left, right, breaking) ->
                    WorldModule.bind5 screen left right breaking world)
                world initializers

        override this.Actualize (screen, world) =
            let views = this.View (this.GetModel screen world, screen, world)
            World.actualizeViews views world

        override this.TrySignal (signalObj, screen, world) =
            match signalObj with
            | :? Signal<'message, obj> as signal -> screen.Signal<'model, 'message, 'command> (match signal with Message message -> msg message | _ -> failwithumf ()) world
            | :? Signal<obj, 'command> as signal -> screen.Signal<'model, 'message, 'command> (match signal with Command command -> cmd command | _ -> failwithumf ()) world
            | _ -> Log.info "Incorrect signal type returned from event binding."; world

        abstract member Initializers : Lens<'model, World> * Screen * World -> PropertyInitializer list
        default this.Initializers (_, _, _) = []

        abstract member Channel : 'model * Screen * World -> Channel<'message, 'command, Screen, World> list
        default this.Channel (_, _, _) = []

        abstract member Message : 'model * 'message * Screen * World -> 'model * Signal<'message, 'command> list
        default this.Message (model, _, _, _) = just model

        abstract member Command : 'model * 'command * Screen * World -> World * Signal<'message, 'command> list
        default this.Command (_, _, _, world) = just world

        abstract member Content : Lens<'model, World> * Screen * World -> LayerContent list
        default this.Content (_, _, _) = []

        abstract member View : 'model * Screen * World -> View list
        default this.View (_, _, _) = []