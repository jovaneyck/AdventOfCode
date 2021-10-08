#r "nuget: Unquote"
open Swensen.Unquote

type Component = int list
type Bridge = Component list

let parse text : Component list =
    let parseComponent (line : string) =
        line.Split([|'/'|])
        |> Seq.map int
        |> Seq.toList
    text |> Seq.map parseComponent |> Seq.toList

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
//input |> Seq.length

let findMatching pins components = components |> List.filter (List.contains pins)
let otherEnd pin (comp : Component) = 
    match comp with
    | [a;b] when a = pin -> b
    | a :: _ -> a
 
let rec buildBridges startPin components : Bridge list =
    match components with
    | [] -> [[]]
    | _ ->
        let options = findMatching startPin components
        match options with
        | [] -> [[]]
        | options -> options |> List.collect (buildStep components startPin)
and
    buildStep components src (comp : Component) = 
        let remaining = components |> List.except [comp]
        let other = otherEnd src comp
        (buildBridges other remaining) |> List.map (fun xs -> comp :: xs)

let components = input |> parse
let all = buildBridges 0 components |> List.distinct

let length (b : Bridge) =
    b |> Seq.length
let strength (b : Bridge) =
    b |> Seq.map Seq.sum |> Seq.sum

let part1 = all |> Seq.map strength |> Seq.max

let maxLength = all |> Seq.map length |> Seq.max
let longestBridges = all |> Seq.filter (fun b -> length b = maxLength)
let part2 = longestBridges |> Seq.map strength |> Seq.max

printf "Testing..."
parse ["1/0";"4/22"] =! [[1;0];[4;22]]
otherEnd 3 [3;4] =! 4
otherEnd 3 [3;3] =! 3
buildBridges 0 [[2;0];[3;2];[2;1]] =! [[[2; 0]; [3; 2]]; [[2; 0]; [2; 1]]]
printfn "..done"
