open System

type LocationType = Wall | OpenSpace | Start | Goal of int
type Coordinate = int * int
type Cell = Coordinate * LocationType
type ParseResult = {start : Cell; goals : Cell list; traversableSpaces : Cell list}

let (|Int|_|) char =
    match Int32.TryParse(string char) with
    | false, _ -> None
    | true, v -> Some <| Int v

let parseCell =
    function
    | Int 0 -> Start
    | Int i -> Goal i
    | '#' -> Wall
    | '.' -> OpenSpace
    | unknown -> failwithf "Unknown cell type: <%c>" unknown

let parse lines =
    [for r in lines |> Seq.indexed ->
        let (rowNumber, row) = r
        [for c in row |> Seq.indexed ->
            let (columnNumber, cell) = c
            ((rowNumber, columnNumber), parseCell cell)]]
    |> Seq.collect id
    |> Seq.toList

let extract parsed =
    let start = parsed |> List.find (function | (_,Start) -> true | _ -> false)
    let goals = parsed |> List.filter (function | (_,Goal _) -> true | _ -> false)
    let openSpaces = parsed |> List.filter (function | (_,OpenSpace) -> true | _ -> false)
    let traversable = start :: goals @ openSpaces
    {start = start; goals = goals; traversableSpaces = traversable}

let otherNodes nodes n =
    let ((x,y),_) = n
    nodes 
    |> List.filter (fun ((ox, oy),_) -> ox > x || oy > y)

let next nodes node = 
    let ((x,y),_) = node

    [(-1,0);(1,0);(0,-1);(0,1)]
    |> List.map (fun (dx,dy) -> (x+dx,y+dy))
    |> List.choose (fun loc -> nodes |> List.tryFind(fun (coord,_) -> coord = loc))

let shortestDistanceBetween nodes startNode endNode =
    //printfn "Calculating distance between %A and %A" startNode endNode
    let rec sp paths visitedNodes distance =
        if paths |> List.isEmpty then None
        elif paths |> List.contains endNode then Some <| distance
        else
            let next = 
                paths
                |> List.collect (next nodes)
                |> List.except visitedNodes
                
            let nextVisited = next @ visitedNodes
            sp next nextVisited (distance + 1)

    sp [startNode] [startNode] 0

let shortestDistances paths nodes =
    let oneWay =
        nodes
        |> List.map (fun n -> (n, otherNodes nodes n))
        |> List.collect 
            (fun (node, otherNodes) -> 
                otherNodes 
                |> List.choose (fun other -> 
                    shortestDistanceBetween paths node other
                    |> Option.map (fun d -> ((node, other), d))))
    let otherWay = 
        oneWay 
        |> List.map (fun ((one,other), distance) -> ((other, one), distance))
    
    oneWay @ otherWay
    |> Map.ofList

let rec distribute e = 
    function
    | [] -> [[e]]
    | h::t as xs -> (e::xs)::[for xs in distribute e t -> h::xs]

let rec permute = 
    function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

let distance distances path =
    let rec d path acc =
        match path with
        | _ :: []
        | [] -> acc
        | a :: b :: rest -> 
            let distAB = distances |> Map.find (a,b) 
            d (b :: rest) (acc + distAB)
    d path 0

//let lines = (@"###########
//#0.1.....2#
//#.#######.#
//#4.......3#
//###########").Split([|'\n'|])

#time
let lines = System.IO.File.ReadLines(System.IO.Path.Combine([| __SOURCE_DIRECTORY__; "input.txt"|]))
let {start = s; goals = g; traversableSpaces = spaces} = lines |> parse |> extract

let p1paths = permute g |> List.map (fun p -> s :: p)
let p2paths = permute g |> List.map (fun p -> s :: p @ [s])

let paths = p1paths //p2paths
let distances = shortestDistances spaces paths.[0]
let pathLengths = paths |> List.map (distance distances)
let pt1 = pathLengths |> List.min
printfn "Part 1: %d" pt1

let paths' = p2paths
let distances' = shortestDistances spaces paths'.[0]
let pathLengths' = paths' |> List.map (distance distances')
let pt2 = pathLengths' |> List.min
printfn "Part 2: %d" pt2

(*
Part 1: 464
Part 2: 652
Real: 00:00:16.203, CPU: 00:00:16.177, GC gen0: 673, gen1: 16, gen2: 0
*)