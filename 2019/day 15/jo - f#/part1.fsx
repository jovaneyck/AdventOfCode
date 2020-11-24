#load "IntCode.fsx"
open IntCode
open IntCode.IntCode

//IntCode.t ()

type StatusCode = HitWall | Moved | FoundOxygenSystem
let toStatusCode =
    function
    | 0L -> HitWall
    | 1L -> Moved
    | 2L -> FoundOxygenSystem
    | u -> failwithf "Unknown status code: %A" u

type Direction = North | South | West | East
let toInput =
    function
    | North -> 1L
    | South -> 2L
    | West -> 3L
    | East -> 4L

type Location = int64 * int64 //hit yourself if you mix up x & y :)
type Tile = Empty | Wall | OxygenSystem
type State = { droid : Location; map : Map<Location, Tile> }
let startState = { droid = (0L,0L); map = Map.empty }

let print { droid = (dx, dy); map = map } =
    let xes = dx :: (map |> Map.toList |> List.map (fst >> fst))
    let ys = dy :: (map |> Map.toList |> List.map (fst >> snd))
    let xmin, xmax = List.min xes, List.max xes
    let ymin, ymax = List.min ys, List.max ys
    [ for y in ymin..ymax do
        for x in xmin..xmax ->
            if (x,y) = (dx, dy) then
                "D"
            else
                let el = map |> Map.tryFind (x,y)
                match el with
                | None -> " "
                | Some p -> match p with | Empty -> "." | OxygenSystem -> "O" | Wall -> "#"
        yield "\n"
    ] |> String.concat ""

//print { droid = (0L,0L) 
//      ; map = ([((2L,0L),Wall);((1L,0L),Empty);((-1L,0L),Wall);((0L,1L),Wall);((0L,-1L),OxygenSystem);] |> Map.ofList) }
//|> printfn "%s"

let location direction (x,y) =
    match direction with
    | North -> (x,y-1L)
    | South -> (x,y+1L)
    | East -> (x+1L,y)
    | West -> (x-1L,y)

let parseDirection = function | "n" -> Some North | "e" -> Some East | "w" -> Some West | "s" -> Some South | _ -> None

let rec getDirection state =
    printfn "Next direction:"
    let i = System.Console.ReadLine() 
    if i = "dump" 
    then 
        printfn "%A" (state.map |> Map.toList |> List.iter (printfn "%A"))
        getDirection state
    else
        match i |> parseDirection with
        | Some d -> d
        | None -> 
            printfn "Unknown direction %A" i
            getDirection state

let tick program state =
    let nextDirection = getDirection state
    let nextLocation = location nextDirection state.droid
    let nextProgram = IntCode.run { program with input = [toInput nextDirection]; output = []; mode = IntCode.Running }
    let statusCode = nextProgram.output |> List.head |> toStatusCode
    let nextState = 
        match statusCode with
        | HitWall -> 
            nextProgram, { state with map = (state.map |> Map.add nextLocation Wall) }
        | FoundOxygenSystem -> 
            nextProgram, { state with map = (state.map |> Map.add nextLocation OxygenSystem) }
        | Moved -> 
            nextProgram, { state with map = (state.map |> Map.add nextLocation Empty) 
                                      droid = nextLocation }
    nextState

let rec loop (p,s) : unit =
    let (np,ns) = tick p s
    print ns |> printf "%s"
    loop (np,ns)

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : IntCode.Program = input.Split([|','|]) |> Seq.map int64 |> Seq.toList
let initProg = (IntCode.initState [] p)

let loaded = initProg |> IntCode.run
let state = startState

loop (loaded, state)