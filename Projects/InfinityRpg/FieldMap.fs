namespace InfinityRpg
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

type FieldUnit =
    { OffsetCount : Vector2i
      IsHorizontal : bool
      PathStart : Vector2i
      PathEnd : Vector2i }

[<RequireQualifiedAccess>]
module FieldMap =

    let PathTile = { TileSheetPositionM = Vector2i (3, 0); TileType = Passable }
    let GrassTile = { TileSheetPositionM = Vector2i (3, 3); TileType = Passable }
    let TreeTile = { TileSheetPositionM = Vector2i (1, 1); TileType = Impassable }

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
        Seq.fold
            (fun (generatedMap, rand) positionM ->
                let (n, rand) = Rand.nextIntUnder 16 rand
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

    let make tileSheet (offsetM : Vector2i) sizeM pathEdgesM rand =
        let buildBoundsM = { CornerNegative = offsetM + Vector2i.One; CornerPositive = offsetM + sizeM - Vector2i.One * 2 }
        let generatedMap = generateEmptyMap offsetM sizeM
        let (generatedMap, rand) = addPaths buildBoundsM pathEdgesM generatedMap rand
        let (generatedMap, rand) = addTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadTrees buildBoundsM generatedMap rand
        let fieldMap = { FieldSizeM = sizeM; FieldTiles = generatedMap; FieldTileSheet = tileSheet }
        (fieldMap, rand)