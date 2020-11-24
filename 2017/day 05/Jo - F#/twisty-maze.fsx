#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let takeStep position offsets = 
    let offset = offsets |> Map.find position
    let newMap = offsets |> Map.add position (offset + 1)
    (position + offset, newMap)

let rec steps stepFn currentPosition offsets acc = 
    if currentPosition >= (offsets |> Seq.length)
    then acc
    else 
        let (nextPos, nextOffsets) = stepFn currentPosition offsets
        let newAcc = acc + 1
        steps stepFn nextPos nextOffsets newAcc

let stepsUntilOut stepFn offsets =
    let offsetMap = 
        offsets
        |> Seq.indexed
        |> Map.ofSeq
    steps stepFn 0 offsetMap 0

let part1 : (int list -> int) = stepsUntilOut takeStep

printf "Testing..."
test <@ part1 [0; 3; 0; 1; -3] = 5 @>
printfn "..done!"

let input = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
    |> Seq.map System.Int32.Parse
    |> Seq.toList

part1 input

let takeStrangerStep position offsets = 
    let offset = offsets |> Map.find position
    let newOffset = 
        offset +
            if offset >= 3 then -1
            else 1
    let newMap = offsets |> Map.add position newOffset
    
    (position + offset, newMap)

let part2 : (int list -> int) = stepsUntilOut takeStrangerStep
test <@ part2 [0; 3; 0; 1; -3] = 10 @>

part2 input