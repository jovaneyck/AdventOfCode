#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type RegionType = Rocky | Narrow | Wet
type Region = { regionType : RegionType; geologicIndex : int }
type Location = { x : int; y : int }
type Cave = { mouth : Location; target : Location; depth : int; grid: Map<Location, Region> }

let tryFind location cave = 
    cave.grid |> Map.tryFind location
let add location region cave = 
    { cave with grid = cave.grid |> Map.add location region }

let rec geologicIndex cave regionLocation =
    let found = cave |> tryFind regionLocation
    match found with
    | Some region -> region.geologicIndex
    | None ->
        match regionLocation with
        | rl when rl = cave.mouth -> 0
        | rl when rl = cave.target -> 0
        | {x = x; y = 0} -> x * 16807 
        | {x = 0; y = y} -> y * 48271
        | {x = x; y = y} -> 
            [ erosionLevel cave { x = x - 1; y = y }
              erosionLevel cave { x = x; y = y - 1 }
            ] |> Seq.reduce (*)
and erosionLevel cave regionLocation =
    let idx = geologicIndex cave regionLocation
    (cave.depth + idx) % 20183

let determineType cave regionLocation =
    let el = erosionLevel cave regionLocation
    match el % 3 with
    | 0 -> Rocky
    | 1 -> Wet
    | _ -> Narrow

let addRegion cave location = 
    let region = { regionType = determineType cave location
                   geologicIndex = geologicIndex cave location }
    cave |> add location region

let buildCave depth targetLocation =
    let { x = tx; y = ty } = targetLocation
    let emptyCave =
        {   target = targetLocation
            depth = depth
            mouth = { x = 0; y = 0 }
            grid = Map.empty }
    [ for y in 0 .. ty do
        for x in 0 .. tx ->
            {x = x; y = y} ]
    |> List.fold addRegion emptyCave

let determineRiskLevel cave = 
    let riskLevelAt region = 
        match region.regionType with
        | Rocky -> 0
        | Wet -> 1
        | Narrow -> 2

    cave.grid
    |> Map.toList
    |> List.map (snd >> riskLevelAt)
    |> List.sum

let tt () =
    let example = buildCave 510 { x = 10; y = 10 }
    example.grid |> Map.find { x = 1; y = 1} |> ignore
    let testRegionAtIs (x,y) expectedRegion cave =
        cave.grid |> Map.find { x = x; y = y} = expectedRegion
    test <@ example |> testRegionAtIs (0,0) { regionType = Rocky; geologicIndex = 0 } @>
    test <@ example |> testRegionAtIs (1,0) { regionType = Wet; geologicIndex = 16807 } @>
    test <@ example |> testRegionAtIs (0,1) { regionType = Rocky; geologicIndex = 48271 } @>
    test <@ example |> testRegionAtIs (1,1) { regionType = Narrow; geologicIndex = 145722555 } @>
    test <@ example |> testRegionAtIs (10,10) { regionType = Rocky; geologicIndex = 0 } @>
    test <@ example |> determineRiskLevel = 114 @>
tt ()
//depth: 11991
//target: 6,797
buildCave 11991 { x = 6; y = 797 } |> determineRiskLevel