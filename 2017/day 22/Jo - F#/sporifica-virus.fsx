#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
open System.IO

//Type-driven development. Let's set up some descriptive types first:
type Location = { X : int; Y : int }
type Direction = Up | Right | Down| Left
type Carrier = { Location : Location; Facing : Direction }
type InfectedNodes = Set<Location>
type State = { Carrier : Carrier; Cluster : InfectedNodes; NumberOfInfectionBursts : int }

let parse text : Location * InfectedNodes = 
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
        |> Seq.choose (fun ((x,y),cell) -> if cell = '#' then Some { X = x; Y = y } else None)
        |> Set.ofSeq
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

let isInfected cluster location = Set.contains location cluster
let clean cluster location = cluster |> Set.remove location
let infect cluster location = cluster |> Set.add location

let moveForward direction ({ X = x; Y = y} as loc) =
    match direction with
    | Up -> { loc with X = x - 1 }
    | Down -> { loc with X = x + 1 }
    | Left -> { loc with Y = y - 1 }
    | Right -> { loc with Y = y + 1 }

let burst {Cluster = cluster; Carrier = carrier; NumberOfInfectionBursts = numberOfInfections} = 
    let currentLocationInfected = isInfected cluster carrier.Location
    let newDirection = 
        if  currentLocationInfected then 
            turnRight carrier.Facing
        else
            turnLeft carrier.Facing
    let newCluster =
        if currentLocationInfected then
            clean cluster carrier.Location
        else
            infect cluster carrier.Location
    let nbOfNewInfections = 
        if not currentLocationInfected then 
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
    |> List.fold (fun state _ -> burst state) state

let solve input =
    input
    |> parse
    |> initialState
    |> run 10000
    |> (fun state -> state.NumberOfInfectionBursts)

let example = @"..#
#..
..."

printf "Testing..."
test <@ parse example = ({ X = 1; Y = 1}, Set.ofList [{ X = 0; Y = 2 }; { X = 1; Y = 0}]) @>
test <@ solve example = 5587 @>
printfn "..done!"

let input = File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt" )
solve input