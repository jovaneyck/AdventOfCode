#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
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

printf "Testing..."
printfn "..done"

let penalty position firewall = 
    firewall
    |> List.tryFind (fun layer -> layer.Depth = position)
    |> Option.filter (fun layer -> layer.Scanner.Position = 1)
    |> Option.map (fun layer -> layer.Depth * layer.Range)
    |> (fun o -> defaultArg o 0)

type State = { Firewall : Layer list; Severity : int }
let step {Firewall = f; Severity = s} position =
    {Firewall = tick f; Severity = s + (penalty position f)}

let moveThrough firewall =
    let ending = (Seq.maxBy (fun l -> l.Depth) firewall).Depth
    [0..ending]
    |> List.fold step {Firewall = firewall; Severity = 0}

let example = @"0: 3
1: 2
4: 4
6: 4"
example |> parse |> moveThrough

//part 1
let input = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt" )
(input |> parse |> moveThrough).Severity