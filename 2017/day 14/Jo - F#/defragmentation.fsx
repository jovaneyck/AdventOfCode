#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let key = "ugkiagan"

let h startIndex startSkipSize list lengths = 
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

let ascii (c : char) = int c
let toLengthSequence input =
    List.append 
        (input
        |> Seq.toList
        |> List.map ascii)

        [17;31;73;47;23]

let knotHash list lengths =
    let map = 
        list
        |> List.indexed
        |> Map.ofList

    [1..64]
    |> List.fold (fun (idx, skipSize, list) _  -> h idx skipSize list lengths) (0,0, map)

let XOR list = list |> Seq.reduce (^^^) //bitwise XOR of every block
let denseHash list = list |> List.chunkBySize 16 |> List.map XOR

let hex (n : int) = n.ToString("X2")


let hash input =
    let (_,_, map) = knotHash [0..255] (toLengthSequence input)
    map
    |> Map.toList
    |> List.map snd
    |> denseHash
    |> List.map hex
    |> String.concat ""
    |> (fun s -> s.ToLower())

let toBinary hexDigit =
    match hexDigit with
    | '0' -> [0;0;0;0]
    | '1' -> [0;0;0;1]
    | '2' -> [0;0;1;0]
    | '3' -> [0;0;1;1]
    | '4' -> [0;1;0;0]
    | '5' -> [0;1;0;1]
    | '6' -> [0;1;1;0]
    | '7' -> [0;1;1;1]
    | '8' -> [1;0;0;0]
    | '9' -> [1;0;0;1]
    | 'a' -> [1;0;1;0]
    | 'b' -> [1;0;1;1]
    | 'c' -> [1;1;0;0]
    | 'd' -> [1;1;0;1]
    | 'e' -> [1;1;1;0]
    | 'f' -> [1;1;1;1]

let binary hexNumber = hexNumber |> Seq.collect toBinary |> Seq.toList

test <@ "899124dac21012ebc32e2f4d11eaec55" = hash "187,254,0,81,169,219,1,190,19,102,255,56,46,32,2,216" @>

test <@ (binary "a0c2017") |> Seq.map string |> String.concat "" = "1010000011000010000000010111" @>
test <@ (binary "2017") |> Seq.map string |> String.concat "" = "0010000000010111" @>
test <@ (binary "1") |> Seq.map string |> String.concat "" = "0001" @>
test <@ (binary "7") |> Seq.map string |> String.concat "" = "0111" @>
test <@ (binary "a0c") |> Seq.map string |> String.concat "" = "101000001100" @>

let hashes =
    [0..127]
    |> List.map (fun row -> sprintf "%s-%d" key row)
    |> List.map hash

//part1
hashes |> List.collect binary |> List.filter (fun b -> b = 1) |> List.length

//part2
let binaryGrid = hashes |> List.map binary

let allLocations grid =
    let dimension = grid |> Seq.length
    [ for r in [0..dimension-1] do
        for c in [0..dimension-1] do
            yield (r,c)]

let isUsed grid (r,c) = 
    grid 
    |> List.tryItem r
    |> Option.bind (fun row -> row |> List.tryItem c |> Option.map (fun v -> v = 1))
    |> (fun o-> defaultArg o false)

let rec buildRegion acc toVisit grid = 
    match toVisit |> Set.toList with
    | [] -> acc
    | (r,c) :: locs ->
        let deltas = [(0,-1); (0,1); (1,0);(-1,0)]
        let neighbours = 
            deltas 
            |> List.map (fun (nr, nc) -> r + nr, c + nc) 
            |> List.filter (isUsed grid)
            |> Set.ofList
        let unvisited = Set.difference neighbours acc
        let newAcc = Set.unionMany [(Set.singleton (r,c)); neighbours; acc]
        buildRegion newAcc (Set.union (Set.ofList locs) unvisited) grid
    

let regions (grid : int list list) = 
    [for (r,c) in allLocations grid do
        if grid.[r].[c] = 1 then
            yield Some <| buildRegion Set.empty (Set.singleton (r,c)) grid
        else 
            yield None
    ] 
    |> List.choose id
    |> Set.ofList

let rs = regions binaryGrid
rs |> Seq.length