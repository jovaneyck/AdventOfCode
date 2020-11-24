//Go grab a coffee, this might take a while :) Brute-forcing the whole thing
open System

type Direction = Up | Down
type Scanner = {Position : int; Facing : Direction}
type Layer = { Depth : int; Range : int; Scanner : Scanner}

let parse (input : string) =
    let parseLine (line : string) =
        let parsed = 
            line.Split([|": "|], StringSplitOptions.RemoveEmptyEntries) 
            |> Array.map int
        {Depth = parsed.[0]; Range = parsed.[1]; Scanner = {Position = 1; Facing = Down}}

    input.Split([|'\n'|])
    |> List.ofSeq
    |> List.map parseLine

let tick firewall =
    let move range scanner =
        let (newPos, newFacing) =
            match scanner.Position, scanner.Facing with
            | 1, Up -> 2, Down
            | p, Down when p = range -> (p - 1, Up)
            | p, Up -> (p - 1, Up)
            | p, Down -> (p + 1, Down)
        {   
            Position = newPos
            Facing = newFacing
        }

    let tickLayer layer =
        let scanner = layer.Scanner
        let nextScanner = move layer.Range scanner

        {layer with Scanner = nextScanner}

    firewall
    |> List.map tickLayer

let penalty position firewall = 
    firewall
    |> List.tryFind (fun layer -> layer.Depth = position)
    |> Option.filter (fun layer -> layer.Scanner.Position = 1)
    |> Option.map (fun layer -> true) 
    |> (fun o -> defaultArg o false)

type State = { Firewall : Layer list; Penalty : bool }
let step state position =
    if state.Penalty then //fold with "short-circuit" when we hit a penalty
        state
    else    
        let {Firewall = f; Penalty = p} = state
        {Firewall = tick f; Penalty = p || (penalty position f)}

let rec foldUntilPenalty stepF state positions =
    match positions with
    | [] -> state
    | p :: ps ->
        if state.Penalty then state
        else
            let next = stepF state p
            foldUntilPenalty stepF next ps

let moveThrough firewall =
    let ending = (Seq.maxBy (fun l -> l.Depth) firewall).Depth
    [0..ending]
    |> foldUntilPenalty step {Firewall = firewall; Penalty = false}