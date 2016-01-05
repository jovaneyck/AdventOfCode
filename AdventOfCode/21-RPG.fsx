printfn "Here-a we go!"

type Cost = int
type Damage = int
type ArmorValue = int
type Name = string

type Spec = {Name : Name; Cost : Cost; Damage : Damage; Armor : ArmorValue;}
type Item = 
    | Weapon of Spec
    | Armor of Spec
    | Ring of Spec

type Equipment = 
    {
        Weapon : Item;
        Armor : Item option;
        Rings : Item list
    }

let makeSpec n c d a = {Name = n; Cost=c;Damage=d;Armor=a}
let makeWeapon n c d a = makeSpec n c d a |> Weapon
let makeArmor n c d a = makeSpec n c d a |> Armor
let makeRing n c d a = makeSpec n c d a |> Ring

let weapons = 
    List.Empty
    |> List.append [makeWeapon "Dagger"     8   4   0]
    |> List.append [makeWeapon "Shortsword" 10  5   0]
    |> List.append [makeWeapon "Warhammer"  25  6   0]
    |> List.append [makeWeapon "Longsword"  40  7   0]
    |> List.append [makeWeapon "Greataxe"   74  8   0]

let armors = 
    List.Empty
    |> List.append [makeArmor "Leather"         13      0   1]
    |> List.append [makeArmor "Chainmail"       31      0   2]
    |> List.append [makeArmor "Splintmail"      53      0   3]
    |> List.append [makeArmor "Bandedmail"      75      0   4]
    |> List.append [makeArmor "Platemail"       102     0   5]

let rings = 
    List.Empty
    |> List.append [makeRing "Damage +1"        25      1   0]
    |> List.append [makeRing "Damage +2"        50      2   0]
    |> List.append [makeRing "Damage +3"        100     3   0]
    |> List.append [makeRing "Defense +1"       20      0   1]
    |> List.append [makeRing "Defense +2"       40      0   2]
    |> List.append [makeRing "Defense +3"       80      0   3]

type HP = int
type Character = {HP : HP; Damage : Damage; Armor : ArmorValue}

let boss = {HP = 100; Damage = 8; Armor = 2}

let (+) {Cost = c1; Damage = d1; Armor = a1} {Cost = c2; Damage = d2; Armor = a2} = 
    {Name = "Totals"; Cost = c1 + c2; Damage = d1 + d2; Armor = a1 + a2}

let totalFolder (acc : Spec) = function
    | Weapon s -> acc + s
    | Armor s -> acc + s
    | Ring s -> acc + s

let toListOfItems {Weapon = w; Armor = a; Rings = r} = w :: ([a] |> List.choose id) @ r
let totals e = toListOfItems e |> List.fold totalFolder {Name = "Totals"; Cost = 0; Armor = 0; Damage = 0}

let singleRings rings = [for r in rings do yield [r]]
let twoRings rings = [
    for r1 in rings do
    for r2 in rings |> List.except [r1] do
    yield [r1; r2]
    ]
let allRingCombinations rings : Item list list = 
    [] :: singleRings rings @ twoRings rings 
    |> List.map (
        fun rings -> 
            rings |> List.sortBy (
                function 
                | Ring({Name=n}) -> n 
                | unknown -> failwith "expected only rings, but got: %A" unknown))
    |> List.distinct

let allLoadouts =
    [
    for w in weapons do
    for a in armors |> List.map Option.Some |> List.append [None] do
    for r in allRingCombinations rings do
    yield {Weapon = w; Armor = a; Rings = r}
    ]

let toCharacter hp equipment = 
    let {Armor = a; Cost = c; Damage = d} = totals equipment
    (c, {HP = hp; Armor = a; Damage = d})

let attackValue dmg armor =
    (dmg - armor) :: [1] |> List.max

//currently performs 2 "hits", one by the opponent, one by the character. Note that this is not 100% the same as the exercise.
//You get the same result as long as you first check the opponent's remaining HP though.
let doBattle 
    (
        {HP = oppHP; Damage = oppDamage; Armor = oppArmor} as opp, 
        ({HP = charHP; Damage = charDamage; Armor = charArmor} as char)
    ) 
    = (
        {opp with HP = oppHP - (attackValue charDamage oppArmor)}, 
        {char with HP = charHP - (attackValue oppDamage charArmor)}
    )

let rec winsAgainst (({HP = oppHP} as opponent), ({HP = charHP} as character)) = 
    if oppHP <= 0 then true
    elif charHP <= 0 then false
    else doBattle (opponent, character) |> winsAgainst

allLoadouts
|> List.map (fun l -> 
    let (cost, char) = toCharacter 100 l
    (l,cost,char))
|> List.filter (fun (_, _, char) -> winsAgainst (boss, char))
|> List.sortBy(fun (_,c,_) -> c)
|> List.head