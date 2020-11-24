#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
open System.IO

//Type-driven development. Let's set up some descriptive types first:
type Location = { X : int; Y : int }
type Direction = Up | Right | Down| Left
type Carrier = { Location : Location; Facing : Direction }
type NodeState = Clean | Weakened | Infected | Flagged
type Cluster = Map<Location, NodeState>
type State = { Carrier : Carrier; Cluster : Cluster; NumberOfInfectionBursts : int }

let parse text : Location * Cluster = 
    let splitLines (text : string) = text.Split([|'\n'|])
    let index grid =
        [
            for rowNumber, row in Seq.indexed grid do
            for columnNumber, column in Seq.indexed row ->
                (rowNumber, columnNumber), column
        ]
    let lines = splitLines text
    let cluster = 
        lines
        |> index
        |> Seq.choose (fun ((x,y),cell) -> if cell = '#' then Some ({ X = x; Y = y }, Infected) else None)
        |> Map.ofSeq
    let middle = Seq.length lines / 2
    ({ X = middle; Y = middle}, cluster)

let initialState (start, cluster) = 
    { Cluster = cluster; Carrier = {Location = start; Facing = Up}; NumberOfInfectionBursts = 0 }

let turnRight = 
    function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let turnLeft = 
    function
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

let getState cluster location =
    match Map.tryFind location cluster with 
    | Some cell -> cell
    | _ -> Clean

let isInfected cluster location = 
    getState cluster location = Infected

let changeState cluster location = 
    match getState cluster location with
    | Clean -> Weakened
    | Weakened -> Infected
    | Infected -> Flagged
    | Flagged -> Clean

let moveForward direction ({ X = x; Y = y} as loc) =
    match direction with
    | Up -> { loc with X = x - 1 }
    | Down -> { loc with X = x + 1 }
    | Left -> { loc with Y = y - 1 }
    | Right -> { loc with Y = y + 1 }

let reverse = 
    function
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

let nextDirection stateOfLocation currentDirection = 
    match stateOfLocation with
    | Clean -> turnLeft currentDirection
    | Weakened -> currentDirection
    | Infected -> turnRight currentDirection
    | Flagged -> reverse currentDirection

let burst {Cluster = cluster; Carrier = carrier; NumberOfInfectionBursts = numberOfInfections} = 
    let stateOfCurrentLocation = getState cluster carrier.Location
    let newDirection = 
        nextDirection stateOfCurrentLocation carrier.Facing
    
    let newCell = changeState cluster carrier.Location
    let newCluster = 
        if newCell = Clean then
            cluster |> Map.remove carrier.Location
        else
            cluster |> Map.add carrier.Location newCell

    let nbOfNewInfections = 
        if stateOfCurrentLocation = Weakened then 
            1
        else
            0
    let newLocation = moveForward newDirection carrier.Location

    { 
        Cluster = newCluster
        Carrier = { Facing = newDirection; Location = newLocation }
        NumberOfInfectionBursts = numberOfInfections + nbOfNewInfections 
    }

let run nb state =
    [1..nb]
    |> List.fold (fun state runnb -> printfn "%d" runnb; burst state) state

let solve runs input =
    input
    |> parse
    |> initialState
    |> run runs
    |> (fun state -> state.NumberOfInfectionBursts)

//let example = @"..#
//#..
//..."

//printf "Testing..."
//test <@ solve 10000000 example = 26 @>
//printfn "..done!"

let input = File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt" )
let result = solve 10000000 input

printfn "Doooone: %d" result