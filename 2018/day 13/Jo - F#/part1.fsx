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
type FoldState = { alreadyMoved : Cart list; stillToMove : Cart list}
type Folder = Going of FoldState | Crash of Location
type TickResult = NoCrash of State | Crashed of Location
let tick grid state = 
    let folder state cart = 
        match state with
        | Crash _ -> state
        | Going state ->
            let moved = move grid cart
            let nextCarts = state.stillToMove |> List.tail
            if state.alreadyMoved @ state.stillToMove |> List.exists (fun c -> c.location = moved.location) 
            then Crash moved.location
            else Going { state with alreadyMoved = moved :: state.alreadyMoved; stillToMove = nextCarts}

    let ticked =
        state.carts
        |> Seq.fold folder (Going { alreadyMoved = []; stillToMove = state.carts |> List.ofSeq })

    match ticked with
    | Crash c -> Crashed c
    | Going {alreadyMoved = next } -> NoCrash { state with carts = next |> List.sortBy (fun c -> c.location) }

let rec untilCrash f x = 
    let next = f x
    match next with
    | Crashed c -> c
    | NoCrash state -> untilCrash f state

let solve input =
    let (grid, initialState) = input |> parse
    untilCrash (tick grid) initialState

let minimal = @"|
v
|
|
|
^
|"

let example = @"/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   "

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

test <@ solve minimal = {x=0;y=3} @>
test <@ solve example = {x=7;y=3} @>

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
input |> solve