#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Row = int list
type Layer = Row list
type Image = Layer list

let dimensions = (25,6)
let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")

let parse (w,h) input : Image =
    let digits = input |> Seq.map (System.Char.GetNumericValue >> int) |> Seq.toList
    let rows = digits |> List.chunkBySize w
    let layers = rows |> List.chunkBySize h
    layers

let layers = 
    input
    |> parse dimensions

let count digit layer = 
    layer 
    |> List.collect id
    |> List.filter (fun d -> d = digit)
    |> List.length

layers |> List.iter (printfn "%A")

let fewest0 =
    layers |> List.minBy (count 0)
let ones = fewest0 |> count 1
let twos = fewest0 |> count 2
let result = ones * twos

printf "Testing..."

test <@ parse (3,2) "123456789012" = [[[1; 2; 3]; [4; 5; 6]]; [[7; 8; 9]; [0; 1; 2]]]  @>

printfn "..done"