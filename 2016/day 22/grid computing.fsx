open System
open System.IO
open System.Text.RegularExpressions

type Location = int * int
type Node = {location : Location; size : int; used : int; available : int }

let parse line = 
    (*
        Filesystem              Size  Used  Avail  Use%
        /dev/grid/node-x0-y0     93T   67T    26T   72%
    *)
    let m = Regex.Match(line, "/dev/grid/node-x(?<x>\d*)-y(?<y>\d*)(\s*)(?<size>\d*)T(\s*)(?<used>\d*)T(\s*)(?<avail>\d*)T(\s*)(\d*)%")
    let intValueOf (groupName : string) = m.Groups.[groupName].Value |> Int32.Parse
    { location = (intValueOf "x", intValueOf "y"); size = intValueOf "size"; used = intValueOf "used"; available = intValueOf "avail" }

let rec pairs predicate list = 
    seq { for x in list -> 
            seq {
                for y in list |> Seq.except (Seq.singleton x) do 
                    if predicate (x,y) then yield (x,y)
            }
    }
    |> Seq.collect id

let inline isEmpty node = node.used = 0
let inline theSame a b = a.location = b.location
let inline fits a b = a.used <= b.available

let isViablePair (a : Node, b : Node) =
       (not <| isEmpty a)
    && (not <| theSame a b)
    && (fits a b)

let emptyNode nodes =
    nodes 
    |> List.find (fun n -> n.used = 0)

#r "..\..\0 lib\F#\Unquote.dll"
open Swensen.Unquote
printfn "Testing..."
parse "/dev/grid/node-x0-y0     93T   67T    26T   72%" =! {location = (0,0); used = 67; available = 26; size = 93}
parse "/dev/grid/node-x38-y19  502T  492T    10T   98%" =! {location = (38,19); used = 492; available = 10; size = 502}

pairs (fun _ -> true) [1;2;3] |> Seq.toList =! [(1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2)]

test <@ not <| isViablePair ({location = (0,0); used = 1; available = 100; size = 0}, {location = (0,0); used = 0; available = 100; size = 0}) @>
test <@ not <| isViablePair ({location = (0,0); used = 0; available = 0; size = 0}, {location = (1,1); used = 0; available = 100; size = 0}) @>
test <@ not <| isViablePair ({location = (0,0); used = 1; available = 0; size = 0}, {location = (1,1); used = 0; available = 0; size = 0}) @>
test <@ isViablePair ({location = (0,0); used = 1; available = 0; size = 0}, {location = (1,1); used = 0; available = 1; size = 0}) @>
printfn "Done!"

let input = File.ReadLines(System.IO.Path.Combine([| __SOURCE_DIRECTORY__; "input.txt"|])) |> Seq.skip 2
//let input = @"/dev/grid/node-x0-y0   10T    8T     2T   80%
///dev/grid/node-x0-y1   11T    6T     5T   54%
///dev/grid/node-x0-y2   32T   28T     4T   87%
///dev/grid/node-x1-y0    9T    7T     2T   77%
///dev/grid/node-x1-y1    8T    0T     8T    0%
///dev/grid/node-x1-y2   11T    7T     4T   63%
///dev/grid/node-x2-y0   10T    6T     4T   60%
///dev/grid/node-x2-y1    9T    8T     1T   88%
///dev/grid/node-x2-y2    9T    6T     3T   66%".Split([|'\n'|])

let nodes = input |> Seq.map parse |> Seq.toList
    
let pt1 = 
    nodes
    |> pairs isViablePair
    |> Seq.length

let toGrid nodes =
    nodes 
    |> Seq.groupBy(fun {location = (x,y)} -> y)
    |> Seq.map snd

let printRow row =
    row
    |> Seq.iter (fun n -> printf "%A %dT/%dT\t" n.location n.used n.size)
    printfn ""

let print grid =
    grid
    |> Seq.iter printRow

nodes |> toGrid |> print

//There's a batch of really large nodes that won't ever get moved, so let's just skip them
let useless nodes =
    nodes 
    |> List.filter(fun n -> List.exists (fun other -> other.size < n.used) nodes)

let usefulNodes = nodes |> List.except (useless nodes)

type State = {free : Location; secret : Location}

let validGridLocations = usefulNodes |> List.map (fun n -> n.location)
let neighboursOf (x,y) =
    [(-1,0);(1,0);(0,-1);(0,1)]
    |> List.map (fun (nx,ny) -> (x + nx, y + ny))
    |> List.filter (fun loc -> validGridLocations |> List.contains loc)

let nextStates {free = free; secret = secret} =
    let neighbours = neighboursOf free
    neighbours
    |> List.map (fun n -> {free = n; secret = (if n = secret then free else secret)})

let isSolution {secret = secretLocation} = secretLocation = (0,0)

//Breadth-first search with visited state tracking
let rec solve step currentStates visitedStates =
    printfn "At step %d" step
    if currentStates |> List.exists isSolution then step
    else
        let newStates = 
            currentStates 
            |> List.collect nextStates 
            |> List.distinct
            |> List.except visitedStates
        solve (step + 1) newStates (newStates @ visitedStates)

let {location = freeLocation} = usefulNodes |> List.find (fun n -> n.used = 0)
let {location = secretLocation} = usefulNodes |> List.filter (fun {location = (_,y)} -> y = 0) |> List.last
let start = {free = freeLocation; secret = secretLocation}
let pt2 = solve 0 [start] [start]