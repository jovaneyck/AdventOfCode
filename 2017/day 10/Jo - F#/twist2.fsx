#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let hash startIndex startSkipSize list lengths = 
    let listLength = list |> Seq.length
    
    let hashLength (index, skipSize, map) length =
        let offsets = [0..length - 1]
        let indices = offsets |> List.map (fun o -> (o + index) % listLength)
        let values = indices |> List.map (fun i -> map |> Map.find i)
        
        let reversed = values |> List.rev
        let updatedMap =
            List.zip indices reversed
            |> List.fold (fun m (i,v) -> m |> Map.add i v) map

        ((index + length + skipSize) % listLength, skipSize + 1, updatedMap)
    
    List.fold hashLength (startIndex, startSkipSize, list) lengths

//part2
let ascii (c : char) = int c
let toLengthSequence input =
    List.append 
        (input
        |> Seq.toList
        |> List.map ascii)

        [17;31;73;47;23]

test <@ toLengthSequence "1,2,3" = [49;44;50;44;51;17;31;73;47;23] @>

let input = "187,254,0,81,169,219,1,190,19,102,255,56,46,32,2,216"

let knotHash list lengths =
    let map = 
        list
        |> List.indexed
        |> Map.ofList

    [1..64]
    |> List.fold (fun (idx, skipSize, list) _  -> hash idx skipSize list lengths) (0,0, map)

let (_,_, map) = knotHash [0..255] (toLengthSequence input)

let XOR list = list |> Seq.reduce (^^^) //bitwise XOR of every block
let denseHash list = list |> List.chunkBySize 16 |> List.map XOR

test <@ XOR [65;27;9;1;4;3;40;50;91;7;6;0;2;5;68;22] = 64 @>

let hex (n : int) = n.ToString("X2")

map
|> Map.toList
|> List.map snd
|> denseHash
|> List.map hex
|> String.concat ""
|> (fun s -> s.ToLower())
