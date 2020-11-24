open System

type Coordinate = {x : int; y : int}

let parse (line : string) = 
    let parseInt = Int32.Parse
    match line.Split([|", "|], StringSplitOptions.None) with
    | [|d1; d2|] -> { x = parseInt d1; y = parseInt d2}
    | _ -> failwithf "parse error"

let buildGrid maxX maxY = 
    [for x in 0..maxX do 
     for y in 0..maxY -> 
        {x = x; y = y}]

let hammingDistance {x = x1; y = y1} {x = x2; y = y2} = 
    Math.Abs(x2 - x1) + Math.Abs(y2 - y1)

type Distance = { point : Coordinate; target : Coordinate; distance : int }
let nearestTarget targets point =
    let pointsWithSmallestDistance =
        targets 
        |> List.map (fun t -> { point = point; target = t; distance = (hammingDistance point t)})
        |> List.groupBy (fun d -> d.distance)
        |> List.minBy fst
        |> snd
    match pointsWithSmallestDistance with
    | [p] -> Some p
    | _ -> None

let findTargetsWithInfiniteArea maxX maxY (distances : Distance list) = 
    let folder problems { point = {x = x; y = y}; target = t} =
        if x = 0 || x = maxX || y = 0 || y = maxY 
        then Set.add t problems
        else problems
    distances
    |> List.fold folder Set.empty

let getPointsInAreaFor closestTargets candidate =
    closestTargets
    |> List.filter (fun t -> t.target = candidate)

let solve input = 
    let targets = input |> List.map parse
    let maxX = targets |> List.map (fun c -> c.x) |> List.max
    let maxY = targets |> List.map (fun c -> c.y) |> List.max
    let grid = buildGrid maxX maxY
    let closestTargets = grid |> List.choose (nearestTarget targets)
    let infinites = findTargetsWithInfiniteArea maxX maxY closestTargets
    let candidates = targets |> List.except infinites
    candidates
    |> List.map (fun candidate -> (candidate, getPointsInAreaFor closestTargets candidate))
    |> List.map (fun (c, pts) -> (c, Seq.length pts))
    |> List.sortByDescending snd
    |> List.head
    |> snd

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let example =  ["1, 1"
                "1, 6"
                "8, 3"
                "3, 4"
                "5, 5"
                "8, 9"]

printf "Testing..."
test <@ parse "142, 265" = { x=142;y=265} @>
test <@ nearestTarget [{x=10;y=10};{x=11;y=9};{x=1;y=1}] {x = 2; y = 2} = Some { point = {x=2;y=2}; target = {x=1;y=1}; distance = 2} @>
test <@ nearestTarget [{x=1;y=1}; {x=3;y=3}] {x = 2; y = 2} = None @>
test <@ solve example = 17 @>
printfn "..done!"

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Seq.toList
solve input