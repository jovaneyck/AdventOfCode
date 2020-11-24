open System.Text.RegularExpressions

type Position = {x : int; y : int}
type Velocity = {dx : int; dy : int}
type Point = { position : Position; velocity : Velocity }

let parse line =
    let pattern = "position=<\s?(-?\d*),\s\s?(-?\d*)> velocity=<\s?(-?\d*),\s\s?(-?\d*)>"
    let m = Regex.Match(line, pattern)
    let parseInt = System.Int32.Parse
    let get (i : int) = m.Groups.[i].Value |> parseInt
    { position = { x = get 1
                   y = get 2}
      velocity = { dx = get 3
                   dy = get 4} }

let tick point = 
    { point with 
        position = 
        { x = point.position.x + point.velocity.dx
          y = point.position.y + point.velocity.dy } }
                
let print points =
    let coordinates = points |> List.map (fun p -> p.position, p) |> Map.ofList
    let xes = points |> List.map (fun p -> p.position.x)
    let ys = points |> List.map (fun p -> p.position.y)
    let (minX, maxX) = xes |> List.min, xes |> List.max
    let (minY, maxY) = ys |> List.min, ys |> List.max

    [for y in [minY..maxY] -> 
        [for x in [minX..maxX] -> 
            if coordinates |> Map.containsKey {x = x; y = y} then "#" else "."]]
    |> Seq.iter (fun l -> l |> String.concat "" |> printfn "%s")

let boundingBoxSize points =
    let xes = points |> List.map (fun p -> p.position.x)
    let ys = points |> List.map (fun p -> p.position.y)
    let (minX, maxX) = xes |> List.min, xes |> List.max
    let (minY, maxY) = ys |> List.min, ys |> List.max
    (uint64 maxX - uint64 minX) * (uint64 maxY - uint64 minY)


let loop points =
    let rec go generation points size =
        let ticked = points |> List.map tick
        let newSize = boundingBoxSize ticked
        
        if newSize > size then (generation, points)
        else go (generation + 1) ticked newSize
        
    go 0 points (boundingBoxSize points)

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let example = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\example.txt") |> List.ofSeq
let ticked =
    example
    |> List.map parse
    |> List.map tick
    |> List.map tick
    |> List.map tick
let (gen, smallest) = example |> List.map parse |> loop
smallest |> print
printf "Testing..."
test <@ parse "position=<-10139,  10477> velocity=< 1, -1>" = { position = {x = -10139; y=10477}; velocity = {dx = 1; dy = -1} } @>
test <@ parse "position=< 31211, -10213> velocity=<-3,  1>" = { position = {x = 31211; y= -10213}; velocity = {dx = -3; dy = 1} } @>
test <@ tick {position = {x =3; y = 4}; velocity = {dx = 2; dy = -3}} = {position = {x =5; y = 1}; velocity = {dx = 2; dy = -3}} @>
printfn "..done!"

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> List.ofSeq
let parsed = input |> List.map parse

let (generation, smallestGrid) = loop parsed
print smallestGrid
generation
