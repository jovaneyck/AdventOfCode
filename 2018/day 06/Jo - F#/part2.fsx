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

let totalDistanceFrom targets point =
    targets
    |> List.map (fun t -> hammingDistance t point)
    |> List.sum

let solve treshold input = 
    let targets = input |> List.map parse
    let maxX = targets |> List.map (fun c -> c.x) |> List.max
    let maxY = targets |> List.map (fun c -> c.y) |> List.max
    let grid = buildGrid maxX maxY
    grid
    |> List.map (fun p -> (p, totalDistanceFrom targets p))
    |> List.filter (fun (p,dist) -> dist < treshold)
    |> List.length
    

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
test <@ solve 32 example = 16 @>
printfn "..done!"

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Seq.toList
solve 10000 input