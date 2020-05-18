namespace InfinityRpg
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Layout =

        let TileSizeI = Vector2i 64
        let TileSize = let t = TileSizeI in t.Vector2
        let TileSheetSizeM = Vector2i 4
        let TileSheetSizeI = Vector2i.Multiply (TileSheetSizeM, TileSizeI)
        let TileSheetSize = let t = TileSheetSizeI in t.Vector2
        let FieldUnitSizeM = Vector2i 22
        let CharacterDepth = 1.0f
        let CharacterWalkSpeed = 4.0f

    [<RequireQualifiedAccess>]
    module InfinityRpg =

        // dissolve constants
        let DissolveDescriptor =
            { IncomingTime = 20L
              OutgoingTime = 30L
              DissolveImage = asset<Image> Assets.GuiPackageName "Dissolve" }

        // splash constants
        let SplashData =
            { DissolveDescriptor = DissolveDescriptor
              IdlingTime = 60L
              SplashImage = asset<Image> Assets.GuiPackageName "Nu" }

        // general data
        let AttackName = "Attack"
        let CharacterAnimationFacingDelay = 16L
        let CharacterAnimationActingDelay = 24L
        let ActionTicksMax = CharacterAnimationActingDelay * 3L