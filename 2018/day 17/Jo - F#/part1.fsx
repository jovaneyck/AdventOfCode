#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
open System.Text.RegularExpressions

type Range = {min : int; max : int }
type ScanData = { x : Range; y : Range }

let parseRange range =
    let rangeRegex = Regex.Match(range, "(.)=(\d*)(\.\.)?(\d*)")
    let get (i : int) = rangeRegex.Groups.[i].Value
    let min = int <| get 2
    let max = if get 4 = "" then min else int <| get 4
    (get 1, { min = min; max = max })

let parseLine line = 
    let lineRegex = Regex.Match(line, "(.*), (.*)")
    let first, second = lineRegex.Groups.[1].Value, lineRegex.Groups.[2].Value
    let [("x", xrange); ("y", yrange)] = [first;second] |> List.map parseRange |> List.sortBy fst
    { x = xrange; y = yrange }

let parse text = 
    text
    |> List.map parseLine

type Location = { x : int; y : int }
type Ground = { clay : Location List; well : Location; activeLocations : Location List; reachable : Location List; still : Location List}
let buildGround scan =
    let toClayLocations (scanData : ScanData) =
        [for y in scanData.y.min .. scanData.y.max do
            for x in scanData.x.min .. scanData.x.max -> {x = x; y = y}]

    scan
    |> List.collect toClayLocations
    |> (fun locations -> 
        let well = { x = 500; y = 0 }
        { clay = locations ; well = well; activeLocations = List.singleton well; reachable = List.empty; still = List.empty})

let spread ground location =
    let bottom = ground.clay |> Seq.maxBy (fun l -> l.y)
    let below : Location = { location with y = location.y + 1 }
    if List.append ground.clay ground.still |> List.contains below |> not 
    then 
        if below.y > bottom.y then ground //out of bounds
        else { ground with activeLocations = below :: ground.activeLocations; reachable = below :: ground.reachable}
    else 
        //try to spread left and right
        let left = { location with x = location.x - 1}
        let right = {location with x = location.x + 1}

        let reachables =
            [left;right] 
            |> List.filter (fun loc -> List.append ground.clay ground.reachable |> List.contains loc |> not)
        { ground with activeLocations = List.append reachables ground.activeLocations; reachable = List.append reachables ground.reachable }

let shouldPool ground location =
    //pool if all water to the left/right & ends with clay + all clay+pooled below
    let leftClay = ground.clay |> Seq.filter (fun cl -> cl.y = location.y && cl.x < location.x) |> Seq.sortByDescending (fun l -> l.x) |> Seq.tryHead
    let rightClay = ground.clay |> Seq.filter (fun cl -> cl.y = location.y && cl.x > location.x) |> Seq.sortBy (fun l -> l.x) |> Seq.tryHead
    match leftClay, rightClay with
    | Some lc, Some rc -> 
        let rowBelow = [ for x in (lc.x + 1) .. (rc.x - 1) -> { x = x; y = location.y + 1} ]
        let pooledBelow = rowBelow |> List.forall (fun loc -> List.append ground.clay ground.still |> Seq.contains loc)
        let toLeftAndRight = [for x in (lc.x + 1) .. (location.x - 1) -> {x = x; y = location.y}] @ [for x in (location.x + 1) .. (rc.x - 1) -> {x = x; y = location.y}]
        let waterToLeftAndRight = toLeftAndRight |> List.forall (fun loc -> ground.reachable |> Seq.contains loc)
        pooledBelow && waterToLeftAndRight
    | _ -> false

let pool ground = 
    //pooled water = water that has Listtled and become "still", every inflow starts spreading again
    let pooled = ground.reachable |> List.filter (shouldPool ground)
    let backActive = pooled |> List.map (fun loc -> {loc with y = loc.y - 1}) |> List.filter (fun loc -> ground.reachable |> Seq.contains loc)
    { ground with 
        reachable = List.except pooled ground.reachable ; 
        still = List.append pooled ground.still; 
        activeLocations = List.append backActive (List.except pooled ground.activeLocations) }

let tick ground = 
    ground.activeLocations
    |> List.fold spread {ground with activeLocations = List.empty}
    |> pool

let print ground =
    let xes = ground.clay |> List.map (fun l -> l.x)
    let ys = ground.clay |> List.map (fun l -> l.y)
    let xmin, xmax = xes |> Seq.min, xes |> Seq.max
    let ymax = ys |> Seq.max
    [ for y in 0.. ymax do
        for x in (xmin-1)..(xmax+1) ->
            let loc = { x = x; y = y }
            if ground.well = loc then "+"
            elif ground.clay |> List.contains loc then "#"
            elif ground.still |> List.contains loc then "~"
            elif ground.reachable |> List.contains loc then "|"
            else "." 
        yield "\n"]
    |> String.concat ""
    |> printfn "%s"

let rec flow nb ground =
    //if nb % 100 = 0 
    //then 
    printfn "%d" nb
    printfn "%A" (ground.activeLocations |> Seq.length, ground.reachable |> Seq.length, ground.still |> Seq.length)
    if List.isEmpty ground.activeLocations then ground
    else flow (nb+1) (tick ground)

let solve input = 
    let doneFlowing = 
        input
        |> parse
        |> buildGround
        |> flow 1
    (doneFlowing.still |> Seq.length) + (doneFlowing.reachable |> Seq.length)

let rec repeat times f x =
    if times = 0 then x
    else repeat (times - 1) f (f x) 

let example = 
    [ 
    "x=495, y=2..7"
    "y=7, x=495..501"
    "x=501, y=3..7"
    "x=498, y=2..4"
    "x=506, y=1..2"
    "x=498, y=10..13"
    "x=504, y=10..13"
    "y=13, x=498..504"]

let tt () = 
    printf "Testing..."
    test <@ parseLine "y=13, x=498..504" = { x = { min = 498; max = 504}; y = { min = 13; max = 13} } @>
    test <@ solve example = 57 @>
    printfn "..done!"

tt ()
//let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> List.ofSeq
//let doneFlowing = 
//        input
//        |> parse
//        |> buildGround
//        |> flow 1
//let part1 = (doneFlowing.still |> Seq.length) + (doneFlowing.reachable |> Seq.length)