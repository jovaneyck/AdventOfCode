open System

let exampleInput = @".#.#.#
...##.
#....#
..#...
#.#..#
####.."

type LightState = 
    | On 
    | Off
type Location = int * int
type Light = Location * LightState
type Grid = Light list

let parseToken rowNb columnNb token : Light =
    let state = 
        match token with
        | '.' -> Off
        | '#' -> On
        | o -> failwith (sprintf "Unknown initial state: %c" o)
    ((rowNb,columnNb), state)

let parseLine lineNb (l : String) =
    let chars = l.ToCharArray()|> List.ofArray
    chars
    |> List.mapi (parseToken lineNb)

let parse (input : String) =
    input.Split('\n')
    |> List.ofArray
    |> List.mapi parseLine
    |> List.collect id

let nextState s nbOnNeighbours =
    match (s, nbOnNeighbours) with
    | (On, 2) -> On
    | (On, 3) -> On
    | (On, _) -> Off
    | (Off, 3) -> On
    | (Off, _) -> Off

let sort (g : Grid) =
    g
    |> List.sortBy (fun l -> l |> fst )

let toTextRow (r : Light list) =
    r
    |> List.map (
        function 
        | _, On -> "#"
        | _, Off -> ".")
    |> List.reduce (+)

let toText (g : Grid) : string =
    g
    |> List.groupBy (fun ((row, _),_) -> row) 
    |> List.map snd
    |> List.map toTextRow
    |> List.reduce (fun a b -> a + "\n" + b)

let render (grid : Grid) = 
    grid
    |> sort
    |> toText
    |> printfn "%s"

let nbLivingNeighbours grid light = 0
let withLivingNeighbours g =
    g
    |> List.map (fun l -> (l, nbLivingNeighbours g l))

let nextGeneration g = g

let step (g : Grid) : Grid =
    g
    |> withLivingNeighbours
    |> nextGeneration
    |> (fun whatever -> g)

let rec repeat n f x =
    if n = 1 then
        f x
    else 
        repeat (n - 1) f (f x)

exampleInput 
|> parse
|> repeat 4 step
|> render