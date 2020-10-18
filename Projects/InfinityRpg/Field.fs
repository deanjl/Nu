namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<StructuralEquality; NoComparison>] Field =
    { FieldMapNp : FieldMap }

    static member initial =
        let DefaultRand = Rand.make ()
        let DefaultSizeM = Vector2i (4, 4)
        let DefaultPathEdgesM = [(Vector2i (1, 1), Vector2i (2, 2))]
        let DefaultFieldMap = fst (FieldMap.make Assets.FieldTileSheetImage (Vector2i.Zero) DefaultSizeM DefaultPathEdgesM DefaultRand)
        { FieldMapNp = DefaultFieldMap }