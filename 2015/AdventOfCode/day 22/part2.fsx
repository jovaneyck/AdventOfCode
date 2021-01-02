#r "nuget: Unquote"
open Swensen.Unquote

//The big idea: do a breadth-first search (BFS), generating all possible moves for the player that make sense (i.e. enough mana, etc).
//Continue down a path until someone hits 0HP or we run out of spells we can cast with our current mana level.
//Once we find a "win", we can start pruning the search space and throw away everything with excess MP consumption.

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

type SpellType =
    | MagicMissile
    | Drain
    | Shield
    | Poison
    | Recharge
let spells = [MagicMissile; Drain; Shield; Poison; Recharge]
let cost = 
    function
    | MagicMissile -> 53
    | Drain -> 73
    | Shield -> 113
    | Poison -> 173
    | Recharge -> 229

type EffectType = 
    | Shielded
    | Poisoned
    | Recharging
type Effect = EffectType * int

type Turn = Player | Boss
type Boss = { hp : int; damage : int }
type Player = { hp : int; mana: int; armor : int }

type State = { turn : Turn; totalManaSpent : int; player : Player; boss : Boss; effects : Effect list }
let init boss = { turn = Player; totalManaSpent = 0; player = { hp = 50; mana = 500; armor = 0 }; boss = boss; effects = [] }

let isActive effect effects = 
    effects |> List.exists (fun e -> fst e = effect)

let ongoingSpells state =
    [   
        if state.effects |> isActive Shielded 
        then Some Shield
        else None;
        if state.effects |> isActive Poisoned 
        then Some Poison
        else None;
        if state.effects |> isActive Recharging 
        then Some Recharge
        else None 
    ] 
    |> List.choose id

let validMoves (state : State) = 
     spells 
     |> List.filter (fun s -> cost s <= state.player.mana)
     |> List.except (ongoingSpells state)

let nextPlayer state =
    let next = 
        match state.turn with 
        | Player -> Boss
        | Boss -> Player
    { state with turn = next }

let bossMove state = 
    let dmg = max 1 (state.boss.damage - state.player.armor)
    [{ state with player = { state.player with hp = state.player.hp - dmg } }]

let tickEffects effects = 
    let effs = effects |> List.map (fun (eff, timer) -> (eff, timer - 1))
    effs

let rec finish effects state =
    match effects with
    | [] -> state
    | Shielded :: es -> finish es { state with player = { state.player with armor = 0 } }
    | e :: es -> finish es state

let applyEffect effect state =
    match effect with
    | Shielded -> state //handled on cast, removed on finish
    | Poisoned -> { state with boss = { state.boss with hp = state.boss.hp - 3 } }
    | Recharging -> { state with player = { state.player with mana = state.player.mana + 101 } }

let rec apply effects state = 
    match effects with
    | [] -> state
    | e :: es -> apply es (applyEffect e state)

let applyEffects state = 
    let ticked = state.effects |> tickEffects
    let finished, active = ticked |> List.partition (fun (e,t) -> t = 0)
    state
    |> apply (ticked |> List.map fst)
    |> finish (finished |> List.map fst) 
    |> fun s -> { s with effects = active }

let cast state spell = 
    let c = cost spell
    match spell with
    | MagicMissile -> 
        { state with 
            totalManaSpent = state.totalManaSpent + c
            player = { state.player with 
                                        mana = state.player.mana - c}
            boss = { state.boss with    
                                        hp = state.boss.hp - 4 } }
    | Drain -> 
        let transfer = if state.boss.hp < 2 then state.boss.hp else 2
        { state with 
            totalManaSpent = state.totalManaSpent + c
            player = { state.player with 
                                        mana = state.player.mana - c
                                        hp = state.player.hp + transfer}
            boss = { state.boss with 
                                        hp = state.boss.hp - transfer } }
    | Shield -> 
        { state with 
            totalManaSpent = state.totalManaSpent + c
            player = { state.player with 
                                        mana = state.player.mana - c
                                        armor = 7 }
            effects = (Shielded, 6) :: state.effects }
    | Poison -> 
        { state with 
            totalManaSpent = state.totalManaSpent + c
            player = { state.player with 
                                        mana = state.player.mana - c }
            effects = (Poisoned, 6) :: state.effects }
    | Recharge -> 
        { state with 
            totalManaSpent = state.totalManaSpent + c
            player = { state.player with 
                                        mana = state.player.mana - c }
            effects = (Recharging, 5) :: state.effects }

let playerMove state = 
    let hardmode = { state with player = { state.player with hp = state.player.hp - 1 } }
    if hardmode.player.hp <= 0 
    then
        [hardmode]
    else
        let moves = validMoves hardmode
        moves |> List.map (cast hardmode)

let playTurn state : State list =
    let afterEffects = state |> applyEffects

    let moved = 
        match afterEffects.turn with
        | Boss -> bossMove afterEffects
        | Player -> playerMove afterEffects
    moved |> List.map nextPlayer

let rec prune opt states =
    match states with
    | [] -> opt
    | s :: ss ->
        let next = playGames opt s |> Option.defaultValue opt
        let nextOptimal = min opt next
        let pruned = ss |> List.filter (fun s -> s.totalManaSpent <= nextOptimal)
        prune nextOptimal pruned
and playGames (optimal : int) state =
    if state.boss.hp <= 0 then 
        //printfn "WIN %d" state.totalManaSpent
        Some state.totalManaSpent //We win!
    else if state.player.hp <= 0 then 
        None //we lose!
    else 
        //We are still playing!
        let candidates = playTurn state
        prune optimal candidates |> Some
        
let inputState = init { hp = 55; damage = 8 }
let part2 = playGames System.Int32.MaxValue inputState

let stateWithMana mana = init { hp = 0; damage = 0} |> fun s -> { s with player = { s.player with mana = mana } }
let stateWithManaAndEffects (mana, effects) = { stateWithMana mana with effects = effects |> List.map (fun e -> (e,1)) }
let testValidMoves input expected =
    test <@ input |> stateWithManaAndEffects |> validMoves |> set = set expected @>

printf "Testing.."
testValidMoves (500, [])                                [MagicMissile; Drain; Shield; Poison; Recharge]
testValidMoves (100, [])                                [MagicMissile; Drain]
testValidMoves (53, [])                                 [MagicMissile]
testValidMoves (52, [])                                 []
testValidMoves (500, [Poisoned])                        [MagicMissile; Drain; Shield; Recharge]
testValidMoves (500, [Shielded])                        [MagicMissile; Drain; Poison; Recharge]
testValidMoves (500, [Recharging])                      [MagicMissile; Drain; Shield; Poison]
testValidMoves (500, [Poisoned; Shielded;Recharging])   [MagicMissile; Drain]
printfn "..done!"