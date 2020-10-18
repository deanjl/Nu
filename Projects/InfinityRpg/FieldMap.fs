﻿namespace InfinityRpg
open System
open Prime
open Nu

type FieldTileType =
    | Impassable
    | Passable

type FieldTile =
    { TileSheetPositionM : Vector2i
      TileType : FieldTileType }

type [<StructuralEquality; NoComparison>] FieldMap =
    { FieldSizeM : Vector2i
      FieldTiles : Map<Vector2i, FieldTile>
      FieldTileSheet : Image AssetTag }

[<RequireQualifiedAccess>]
module FieldMap =

    let PathTile = { TileSheetPositionM = Vector2i (3, 0); TileType = Passable }
    let GrassTile = { TileSheetPositionM = Vector2i (3, 3); TileType = Passable }
    let TreeTile = { TileSheetPositionM = Vector2i (1, 1); TileType = Impassable }
    let StoneTile = { TileSheetPositionM = Vector2i (2, 3); TileType = Impassable }
    let WaterTile = { TileSheetPositionM = Vector2i (0, 0); TileType = Impassable }

    let makeGrid boundsM =
        seq {
            for i in boundsM.CornerNegative.X .. boundsM.CornerPositive.X do
                for j in boundsM.CornerNegative.Y .. boundsM.CornerPositive.Y do
                    yield Vector2i (i, j) }

    let generateEmptyMap (offsetM : Vector2i) (sizeM : Vector2i) =
        Map.ofList
            [for i in offsetM.X .. offsetM.X + sizeM.X - 1 do
                for j in offsetM.Y .. offsetM.Y + sizeM.Y - 1 do
                    let tileCoordsM = Vector2i (i, j)
                    yield (tileCoordsM, GrassTile)]

    let addPaths buildBoundsM pathEdgesM generatedMap rand =
        
        let (paths, rand) =
            List.fold
                (fun (paths, rand) (sourceM, destinationM) ->
                    let (path, rand) = Direction.wanderToDestination buildBoundsM sourceM destinationM rand
                    (path :: paths, rand))
                ([], rand)
                pathEdgesM

        let generatedMap =
            Seq.fold
                (fun generatedMap path ->
                    let generatedMap' =
                        Seq.fold
                            (fun generatedMap tilePositionM -> Map.add tilePositionM PathTile generatedMap)
                            generatedMap
                            path
                    generatedMap @@ generatedMap')
                generatedMap
                paths

        (generatedMap, rand)

    let addTrees buildBoundsM generatedMap rand =
        let grid = makeGrid buildBoundsM
        let pathTileCount = Map.filter (fun _ v -> v = PathTile) generatedMap |> Map.count
        let treeDilution =
            if pathTileCount < 25 then 32
            elif pathTileCount < 50 then 16
            elif pathTileCount < 60 then 8
            elif pathTileCount < 70 then 4
            elif pathTileCount < 80 then 3
            elif pathTileCount < 90 then 2
            else 1
        Seq.fold
            (fun (generatedMap, rand) positionM ->
                let (n, rand) = Rand.nextIntUnder treeDilution rand // original value is 16
                if n = 0 && Map.find positionM generatedMap <> PathTile
                then (Map.add positionM TreeTile generatedMap, rand)
                else (generatedMap, Rand.advance rand))
            (generatedMap, rand)
            grid

    let spreadTrees buildBoundsM generatedMap rand =
        let originalMap = generatedMap
        let grid = makeGrid buildBoundsM
        Seq.fold
            (fun (generatedMap, rand) positionM ->
                let tile = Map.find positionM originalMap
                if  tile <> PathTile &&
                    MapBounds.isPointInBounds positionM buildBoundsM then
                    let upPositionM = positionM + Vector2i.Up
                    let rightPositionM = positionM + Vector2i.Right
                    let downPositionM = positionM + Vector2i.Down
                    let leftPositionM = positionM + Vector2i.Left
                    if  MapBounds.isPointInBounds upPositionM buildBoundsM && Map.find upPositionM originalMap = TreeTile ||
                        MapBounds.isPointInBounds rightPositionM buildBoundsM && Map.find rightPositionM originalMap = TreeTile ||
                        MapBounds.isPointInBounds downPositionM buildBoundsM && Map.find downPositionM originalMap = TreeTile ||
                        MapBounds.isPointInBounds leftPositionM buildBoundsM && Map.find leftPositionM originalMap = TreeTile then
                        let (n, rand) = Rand.nextIntUnder 3 rand
                        if n = 0 then (Map.add positionM TreeTile generatedMap, rand)
                        else (generatedMap, Rand.advance rand)
                    else (generatedMap, Rand.advance rand)
                else (generatedMap, Rand.advance rand))
            (generatedMap, rand)
            grid

    let addWater buildBoundsM generatedMap rand =
        let pathTileCount = Map.filter (fun _ v -> v = PathTile) generatedMap |> Map.count
        if pathTileCount < 25 then
            let grid = makeGrid buildBoundsM
            Seq.fold
                (fun (generatedMap, rand) positionM ->
                    let (n, rand) = Rand.nextIntUnder 128 rand
                    let upPositionM = positionM + Vector2i.Up
                    let rightPositionM = positionM + Vector2i.Right
                    let downPositionM = positionM + Vector2i.Down
                    let leftPositionM = positionM + Vector2i.Left
                    if  MapBounds.isPointInBounds upPositionM buildBoundsM && Map.find upPositionM generatedMap = GrassTile &&
                        MapBounds.isPointInBounds rightPositionM buildBoundsM && Map.find rightPositionM generatedMap = GrassTile &&
                        MapBounds.isPointInBounds downPositionM buildBoundsM && Map.find downPositionM generatedMap = GrassTile &&
                        MapBounds.isPointInBounds leftPositionM buildBoundsM && Map.find leftPositionM generatedMap = GrassTile then
                        if n = 0 && Map.find positionM generatedMap = GrassTile
                        then (Map.add positionM WaterTile generatedMap, rand)
                        else (generatedMap, rand)
                    else (generatedMap, rand))
                (generatedMap, rand)
                grid
        else (generatedMap, rand)

    let spreadWater buildBoundsM generatedMap rand =
        let pathTileCount = Map.filter (fun _ v -> v = PathTile) generatedMap |> Map.count
        if pathTileCount < 25 then
            let grid = makeGrid buildBoundsM
            Seq.fold
                (fun (generatedMap, rand) positionM ->
                    let (n, rand) = Rand.nextIntUnder 2 rand
                    let upPositionM = positionM + Vector2i.Up
                    let rightPositionM = positionM + Vector2i.Right
                    let downPositionM = positionM + Vector2i.Down
                    let leftPositionM = positionM + Vector2i.Left
                    if  MapBounds.isPointInBounds upPositionM buildBoundsM && Map.find upPositionM generatedMap = WaterTile ||
                        MapBounds.isPointInBounds rightPositionM buildBoundsM && Map.find rightPositionM generatedMap = WaterTile ||
                        MapBounds.isPointInBounds downPositionM buildBoundsM && Map.find downPositionM generatedMap = WaterTile ||
                        MapBounds.isPointInBounds leftPositionM buildBoundsM && Map.find leftPositionM generatedMap = WaterTile then
                        if n = 0 && Map.find positionM generatedMap <> PathTile
                        then (Map.add positionM WaterTile generatedMap, rand)
                        else (generatedMap, rand)
                    else (generatedMap, rand))
                (generatedMap, rand)
                grid
        else (generatedMap, rand)

    let addStones buildBoundsM generatedMap rand =
        let grid = makeGrid buildBoundsM
        Seq.fold
            (fun (generatedMap, rand) positionM ->
                if Map.find positionM generatedMap = GrassTile then
                    let upPositionM = positionM + Vector2i.Up
                    let rightPositionM = positionM + Vector2i.Right
                    let downPositionM = positionM + Vector2i.Down
                    let leftPositionM = positionM + Vector2i.Left
                    if  Map.find upPositionM generatedMap = PathTile &&
                        Map.find rightPositionM generatedMap = PathTile &&
                        Map.find downPositionM generatedMap = PathTile &&
                        Map.find leftPositionM generatedMap = PathTile then
                        (Map.add positionM StoneTile generatedMap, Rand.advance rand)
                    else (generatedMap, Rand.advance rand)
                else (generatedMap, Rand.advance rand))
            (generatedMap, rand)
            grid
    
    let make tileSheet (offsetM : Vector2i) sizeM pathEdgesM rand =
        let buildBoundsM = { CornerNegative = offsetM + Vector2i.One; CornerPositive = offsetM + sizeM - Vector2i.One * 2 }
        let generatedMap = generateEmptyMap offsetM sizeM
        let (generatedMap, rand) = addPaths buildBoundsM pathEdgesM generatedMap rand
        let (generatedMap, rand) = addTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = addWater buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadWater buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadWater buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadWater buildBoundsM generatedMap rand
        let (generatedMap, rand) = addStones buildBoundsM generatedMap rand
        let fieldMap = { FieldSizeM = sizeM; FieldTiles = generatedMap; FieldTileSheet = tileSheet }
        (fieldMap, rand)