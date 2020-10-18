﻿namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

[<RequireQualifiedAccess>]
module Algorithms =

    let expReqs =
        [0; 8
         15; 22; 35; 50; 75; 100
         150; 220; 350; 500; 750; 1000
         1500; 2200; 3500; 5000; 7500; 10000
         15000; 22000; 35000; 50000; 75000; 100000
         150000; 220000; 350000; 500000; 750000; 1000000]

    let expReqRanges =
        List.pairwise expReqs

    let levelMax =
        List.length expReqs

    let levelToExpPointsRange level =
        expReqRanges |>
        List.tryItem (dec level) |> // level 1 is the minimum
        Option.getOrDefault (List.last expReqs, Int32.MaxValue)

    let levelToExpPoints level =
        fst (levelToExpPointsRange level)

    let expPointsToLevel expPoints =
        expReqRanges |>
        List.tryFindIndex (fun (low, high) -> expPoints >= low && expPoints < high) |>
        Option.map inc |> // level 1 is the minimum
        Option.getOrDefault levelMax

    let hitPointsMax armorOpt archetypeType level =
        let stamina = 
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Stamina
            | None -> 1.0f
        let intermediate =
            match armorOpt with
            | Some armor ->
                match Map.tryFind armor data.Value.Armors with
                | Some armorData -> single armorData.HitPointsBase
                | None -> 8.0f
            | None -> 8.0f
        intermediate * single level * stamina |> int |> max 1

    let techPointsMax armorOpt archetypeType level =
        let focus = 
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Focus
            | None -> 1.0f
        let intermediate =
            match armorOpt with
            | Some armor ->
                match Map.tryFind armor data.Value.Armors with
                | Some armorData -> single armorData.TechPointsBase
                | None -> 4.0f
            | None -> 4.0f
        intermediate * single level * focus |> int |> max 0

    let power weaponOpt powerBuff archetypeType level =
        let strength = 
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Strength
            | None -> 1.0f
        let intermediate =
            match weaponOpt with
            | Some weapon ->
                match Map.tryFind weapon data.Value.Weapons with
                | Some weaponData -> single weaponData.PowerBase
                | None -> 1.0f
            | None -> 1.0f
        intermediate * single level * powerBuff * strength |> int |> max 1

    let magic weaponOpt magicBuff archetypeType level =
        let intelligence = 
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Intelligence
            | None -> 1.0f
        let intermediate =
            match weaponOpt with
            | Some weapon ->
                match Map.tryFind weapon data.Value.Weapons with
                | Some weaponData -> single weaponData.MagicBase
                | None -> 1.0f
            | None -> 1.0f
        intermediate * single level * magicBuff * intelligence |> int |> max 1

    let shield effectType accessories shieldBuff archetypeType level =
        let (toughness, intelligence) = 
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> (archetypeData.Toughness, archetypeData.Intelligence)
            | None -> (1.0f, 1.0f)
        let intermediate =
            match accessories with
            | accessory :: _ -> // just the first accessory for now
                match Map.tryFind accessory data.Value.Accessories with
                | Some accessoryData -> single accessoryData.ShieldBase
                | None -> 0.0f
            | _ -> 0.0f
        let scalar = match effectType with Magical -> intelligence | Physical -> toughness
        intermediate * single level * shieldBuff * scalar |> int |> max 0

    let techs archetypeType level =
        let techs =
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Techs
            | None -> Map.empty
        let indexOpt = techs |> Map.toList |> List.tryFindIndexBack (fun (levelReq, _) -> level >= levelReq)
        match indexOpt with
        | Some index -> techs |> Map.toList |> List.take (inc index) |> List.map snd |> Set.ofList
        | None -> Set.empty

    let goldPrize scalar (level : int) =
        let algo = level * level // sure, why not?
        int (single algo * scalar)

    let expPrize scalar (level : int) =
        let algo = level * level // sure, why not?
        int (single algo * scalar)