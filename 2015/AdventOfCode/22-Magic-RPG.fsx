type Damage = int
type HP = int
type MP = int
type Duration = int

type Player = {HP : HP; MP : MP}
type Boss = {HP : HP; Damage : Damage}

type ActivePlayer =
    | Boss
    | Player

type Effect = 
    | Shield
    | Poison
    | Recharge
type ActiveEffect = {effect : Effect; remainingTime : Duration}

type Spell =
    | MagicMissile
    | Shield
    | Poison
    | Recharge

type SpellSpecification = {spell : Spell; cost : MP}

type MomentInTime = {Boss : Boss; Character : Player; ActivePlayer : ActivePlayer; ActiveEffects : ActiveEffect list; ManaSpent : MP}

let startingMoment = {
        ActivePlayer = Player; 
        Boss = {HP = 55; Damage = 8}; 
        Character = {HP = 50; MP = 500}; 
        ManaSpent = 0;
        ActiveEffects = []
    }

let applyDamageTo {Damage = d} effects ({HP = hp} as character : Player) =
    {character with HP = hp - d} //TODO: incorporate effects!

let playerMove m = m //TODO

let nextMoment ({ActivePlayer = ap; Boss = b; Character = c; ActiveEffects = eff} as moment) = 
    match ap with
    | Boss -> {moment with Character = applyDamageTo b eff c; ActivePlayer = Player}
    | Player -> {playerMove moment with ActivePlayer = Boss}

startingMoment |> nextMoment |> nextMoment