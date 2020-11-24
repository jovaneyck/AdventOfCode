#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
open System

let split (delimiter : string) (text : string) =
    text.Split([|delimiter|], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofSeq

type DirectConnections = (int*int) list

let parse input : DirectConnections =
    let parseInt = Int32.Parse
    let parseLine line =
        let src :: destinations :: _ = split " <-> " line
        destinations
        |> split ","
        |> List.map (fun d -> (parseInt src, parseInt d))

    input
    |> split "\n"
    |> List.collect parseLine

let directNeighbours location connections =
    connections 
    |> List.filter (fun (src, _) -> src = location)
    |> List.map snd

let rec reachableFrom location connections = 
    let neighbours = directNeighbours location connections
    location :: neighbours

let rec fixpoint f x =
    let y = f x
    if x = y 
    then x
    else fixpoint f y

let expand (connections : DirectConnections) group =
    let step = group |> List.collect (fun element -> reachableFrom element connections)
    group @ step |> List.distinct

let solve start connections = fixpoint (expand connections) [start]

//let input = @"0 <-> 1,2
//1 <-> 2,3
//2 <-> 4
//3 <-> 3
//4 <-> 2,5
//5 <-> 5"
let input = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt")

let parsed =
    input
    |> parse

//part 1
//parsed
//|> solve 0
//|> List.length

//part 2
let rec findGroups acc connections =
    match connections with
    | [] -> acc
    | (src,_) :: _ ->
        let group = solve src connections
        let remainingConnections = connections |> List.filter (fun (s,d) -> group |> List.contains d |> not && group |> List.contains s |> not)
        findGroups (group :: acc) remainingConnections

findGroups [] parsed
|> List.length