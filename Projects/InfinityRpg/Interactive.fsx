﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

#I __SOURCE_DIRECTORY__
#r "../../packages/Magick.NET-Q8-x64.7.16.1/lib/net40/Magick.NET-Q8-x64.dll"
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsec.dll"
#r "../../packages/FsCheck.2.11.0/lib/net452/FsCheck.dll"
#r "../../packages/FsCheck.Xunit.2.11.0/lib/net452/FsCheck.Xunit.dll"
#r "../../packages/Prime.5.5.0/lib/net472/Prime.dll"
#r "../../packages/Prime.Scripting.5.1.0/lib/net472/Prime.Scripting.exe"
#r "../../Nu/Nu.Dependencies/FSharpx.Core/FSharpx.Core.dll"
#r "../../Nu/Nu.Dependencies/FSharpx.Collections/FSharpx.Collections.dll"
#r "../../Nu/Nu.Dependencies/Farseer/FarseerPhysics.dll"
#r "../../Nu/Nu.Dependencies/Nito.Collections.Deque/Nito.Collections.Deque.dll"
#r "../../Nu/Nu.Dependencies/SDL2-CS/Debug/SDL2-CS.dll"
#r "../../Nu/Nu.Dependencies/TiledSharp/Debug/TiledSharp.dll"
#r "../../Nu/Nu.Astar/bin/Debug/Nu.Astar.dll"
#r "../../Nu/Nu.Math/bin/x64/Debug/Nu.Math.dll"
#r "../../Nu/Nu/bin/Debug/Nu.exe"

#load "InfinityAssets.fs"
#load "InfinityConstants.fs"
#load "InfinitySimulants.fs"
#load "InfinityMath.fs"
#load "CharacterActivityState.fs"
#load "CharacterAnimationState.fs"
#load "CharacterState.fs"
#load "MetaMap.fs"
#load "FieldMap.fs"
#load "FieldDispatcher.fs"
#load "CharacterFacets.fs"
#load "CharacterDispatcher.fs"
#load "EnemyDispatcher.fs"
#load "PlayerDispatcher.fs"
#load "OccupationMap.fs"
#load "GameplayDispatcher.fs"
#load "InfinityDispatcher.fs"
#load "InfinityPlugin.fs"

open System.IO
open Prime
open Nu

// ensure project has been built at least once before proceeding
let workingDirPath = __SOURCE_DIRECTORY__ + "/bin/Debug"
if not (Directory.Exists workingDirPath) then failwith "You must build the project at least once before running in interactive."
Directory.SetCurrentDirectory workingDirPath

// copy over required project files
File.Copy ("../../AssetGraph.nuag", "AssetGraph.nuag", true)
File.Copy ("../../Overlayer.nuol", "Overlayer.nuol", true)
File.Copy ("../../Prelude.nuscript", "Prelude.nuscript", true)

// build assets
match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
| Right assetGraph -> AssetGraph.buildAssets "../.." "." "../../refinement" false assetGraph
| Left _ -> ()

// init nu and run game
Nu.init NuConfig.defaultConfig
World.run WorldConfig.defaultConfig (InfinityRpg.InfinityPlugin ())