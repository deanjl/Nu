namespace InfinityRpg

type CharacterAnimationType =
    | CharacterAnimationFacing
    | CharacterAnimationActing
    | CharacterAnimationDefending
    | CharacterAnimationSpecial // works for jump, cast magic, being healed, and perhaps others!
    | CharacterAnimationSlain

type CharacterAnimationState =
    { StartTime : int64
      AnimationType : CharacterAnimationType
      Direction : Direction }
    
    static member initial =
        { StartTime = 0L
          AnimationType = CharacterAnimationFacing
          Direction = Upward }