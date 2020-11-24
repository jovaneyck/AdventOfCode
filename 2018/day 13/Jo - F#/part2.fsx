type Location = {x : int; y : int}
type Track = Vert | Hor | CurveRight | CurveLeft | Intersection
type Action = GoLeft | GoStraight | GoRight
type Orientation = N | E | W | S
type Cart = { location : Location; orientation : Orientation; nextAction : Action }
type State = {carts : Cart list}

let makeCart location orientation =
    { location = location; orientation = orientation; nextAction = GoLeft }

let parseLocation (row,col,character) = 
    let pos =  {x = col; y = row }
    
    match character with
    | ' ' -> None
    | '+' -> Some ((pos, Intersection), [])
    | '|' -> Some ((pos, Vert), [])
    | '-' -> Some ((pos, Hor), [])
    | '/' -> Some ((pos, CurveRight), [])
    | '\\' -> Some ((pos, CurveLeft), [])
    | '<' -> Some ((pos, Hor), [ makeCart pos W ])
    | '>' -> Some ((pos, Hor), [ makeCart pos E ])
    | '^' -> Some ((pos, Vert), [ makeCart pos N ])
    | 'v' -> Some ((pos, Vert), [ makeCart pos S ])
    | unknown -> failwithf "UNKNOWN TOKEN: %A" unknown

let parse (input : string) =
    let (tracks, parsedCarts) =
        input.Split([|'\r';'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.toList
        |> List.indexed
        |> List.collect (fun (row,characters) -> characters |> Seq.toList |> List.indexed |> List.map (fun (col, character) -> (row,col,character) ) )
        |> List.choose parseLocation
        |> List.unzip
    let grid = tracks |> Map.ofList
    let carts = parsedCarts |> List.collect id
    let state = {carts = carts}
    (grid, state)

let nextLocation cart =
    match cart.orientation with
    | N -> { cart.location with y = cart.location.y - 1 }
    | S -> { cart.location with y = cart.location.y + 1 }
    | E -> { cart.location with x = cart.location.x + 1 }
    | W -> { cart.location with x = cart.location.x - 1 }

let find location grid = grid |> Map.find location
let rotateLeftCurve =
    // \
    function
    | N -> W
    | E -> S
    | S -> E
    | W -> N
    
let rotateRightCurve =
    // /
    function
    | N -> E
    | E -> N
    | S -> W
    | W -> S

let turnCounterClockwise =
    function
    | N -> W
    | E -> N
    | S -> E
    | W -> S

let turnClockwise =
    function
    | N -> E
    | E -> S
    | S -> W
    | W -> N


let onCorner cart =
    match cart.nextAction with
    | GoLeft -> { cart with nextAction = GoStraight; orientation = turnCounterClockwise cart.orientation }
    | GoRight -> { cart with nextAction = GoLeft; orientation = turnClockwise cart.orientation }
    | GoStraight -> { cart with nextAction = GoRight }

let move grid cart = 
    let nextLoc = nextLocation cart
    let moved = { cart with location = nextLoc }
    let nextObstacle = grid |> find nextLoc
    match nextObstacle with
    | Vert
    | Hor -> moved
    | CurveLeft -> {moved with orientation = rotateLeftCurve cart.orientation}
    | CurveRight -> {moved with orientation = rotateRightCurve cart.orientation}
    | Intersection -> moved |> onCorner

//Some multi-list hocus pocus to handle non-simultaneous movement of the carts
type FoldState = { alreadyMoved : Cart list; stillToMove : Cart list; crashes : Cart list}
let tick grid state = 
    let folder state cart = 
        let nextCarts = if state.stillToMove |> List.isEmpty then [] else state.stillToMove |> List.tail
        if state.crashes |> List.contains cart then
            { state with stillToMove = nextCarts }
        else
            let moved = move grid cart
            let newCrashes = state.alreadyMoved @ nextCarts |> List.filter (fun c -> c.location = moved.location) 

            if newCrashes |> List.isEmpty |> not then
                let allNewCrashes = cart :: newCrashes
                { state with
                    alreadyMoved = state.alreadyMoved |> List.except allNewCrashes
                    stillToMove = nextCarts |> List.except allNewCrashes
                    crashes = allNewCrashes @ state.crashes }
            else
                { state with 
                    alreadyMoved = moved :: state.alreadyMoved
                    stillToMove = nextCarts}

    let next =
        state.carts
        |> Seq.fold folder { alreadyMoved = []; stillToMove = state.carts |> List.ofSeq; crashes = [] }

    { state with carts = next.alreadyMoved |> List.sortBy (fun c -> c.location) }

let rec untilSingleCart f x = 
    let next = f x
    
    if next.carts |> Seq.length = 1 then next.carts |> List.head |> (fun c -> c.location)
    else untilSingleCart f next

let solve input =
    let (grid, initialState) = input |> parse
    untilSingleCart (tick grid) initialState

let example = @"/>-<\  
|   |  
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/"

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

test <@ solve example = {x=6;y=4} @>

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
input |> solve //88,64