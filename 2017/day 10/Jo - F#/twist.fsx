#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let hash list lengths = 
    let listLength = list |> List.length
    
    let hashLength (index, skipSize, map) length =
        let offsets = [0..length - 1]
        let indices = offsets |> List.map (fun o -> (o + index) % listLength)
        let values = indices |> List.map (fun i -> map |> Map.find i)
        
        let reversed = values |> List.rev
        let updatedMap =
            List.zip indices reversed
            |> List.fold (fun m (i,v) -> m |> Map.add i v) map

        ((index + length + skipSize) % listLength, skipSize + 1, updatedMap)

    let map = 
        list
        |> List.indexed
        |> Map.ofList
    
    let (_, _, hashed) =
        List.fold hashLength (0, 0, map) lengths

    hashed
    |> Map.toList
    |> List.sortBy fst
    |> List.map snd


let example = [0..4]
let exampleLengths = [3;4;1;5]

test <@ hash example exampleLengths = [3;4;2;1;0] @>

let input = [187;254;0;81;169;219;1;190;19;102;255;56;46;32;2;216]

//part 1
let hashed = hash [0..255] input
hashed.[0] * hashed.[1]