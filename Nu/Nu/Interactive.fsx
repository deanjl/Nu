﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

#I __SOURCE_DIRECTORY__
#r "../../packages/Magick.NET-Q8-x64.7.16.1/lib/net40/Magick.NET-Q8-x64.dll"
#r "../../packages/TiledSharp.1.0.1/lib/netstandard2.0/TiledSharp.dll"
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsec.dll"
#r "../../packages/FsCheck.2.11.0/lib/net452/FsCheck.dll"
#r "../../packages/FsCheck.Xunit.2.11.0/lib/net452/FsCheck.Xunit.dll"
#r "../../packages/Prime.5.25.0/lib/net472/Prime.dll"
#r "../../packages/Prime.Scripting.5.3.0/lib/net472/Prime.Scripting.exe"
#r "../../packages/FSharpx.Core.1.8.32/lib/40/FSharpx.Core.dll"
#r "../../packages/FSharpx.Collections.2.1.3/lib/net45/FSharpx.Collections.dll"
#r "../../packages/FarseerPhysics.3.5.0/lib/NET40/FarseerPhysics.dll"
#r "../../packages/Nito.Collections.Deque.1.1.0/lib/netstandard2.0/Nito.Collections.Deque.dll"
#r "../../packages/SDL2-CS.dll.2.0.0.0/lib/net20/SDL2-CS.dll"
#r "../../Nu/Nu.Math/bin/x64/Debug/Nu.Math.dll"
#r "../../Nu/Nu/bin/Debug/Nu.exe"

open System
open System.IO
open FSharpx
open FSharpx.Collections
open SDL2
open TiledSharp
open Prime
open Nu
open Nu.Declarative

// set current directly to local for execution in VS F# interactive
Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__ + "/bin/Debug")

// initialize Nu
Nu.init NuConfig.defaultConfig