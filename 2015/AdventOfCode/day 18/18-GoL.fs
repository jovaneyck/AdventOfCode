module GoL

open System

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
    l.ToCharArray()
    |> List.ofArray
    |> List.mapi (parseToken lineNb)

let parse (input : String) =
    input.Split('\n')
    |> List.ofArray
    |> List.map (fun l -> l.Trim())
    |> List.mapi parseLine
    |> List.collect id

let nextState dimension (x,y) s nbOnNeighbours =
    if [(0,0); (0, dimension - 1); (dimension - 1, 0); (dimension - 1, dimension - 1)] |> List.contains (x,y)
    then On
    else
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

let allPossibleNeighbours ((x,y) : Location) : Location list = 
    [ (x-1, y+1); (x, y+1); (x+1, y+1);
      (x-1, y);             (x+1, y);
      (x-1, y-1); (x, y-1); (x+1, y-1)]

let dimension g =
    g |> List.length |> (float) |> Math.Sqrt |> (int)

let livingAt (grid : Grid) (x,y) = 
    let d = dimension grid 
    
    if(x < 0 || y < 0 || x >= d || y >= d) 
    then false
    else 
        let index = x * d + y
        let (loc,state) = grid.[index]
        state = On

let nbLivingNeighbours grid loc = 
    loc
    |> allPossibleNeighbours
    |> List.map (livingAt grid)
    |> List.filter ((=) true)
    |> List.length

let withLivingNeighbours g =
    g
    |> List.map (fun ((loc, state) as light) -> (light, nbLivingNeighbours g loc))

let nextGeneration (lightsWithLivingNeighbours : (Light * int) list) = 
    let dim = dimension lightsWithLivingNeighbours
    lightsWithLivingNeighbours
    |> List.map (fun ((loc, state), nbLivingNeighbours) -> (loc, nextState dim loc state nbLivingNeighbours))

let step (g : Grid) : Grid =
    g
    |> withLivingNeighbours
    |> nextGeneration

let rec repeat n f x =
    printfn "Generation %d" n
    if n = 1 then
        f x
    else 
        repeat (n - 1) f (f x)

let printNbLightsOn g = 
    g 
    |> List.filter (fun (_, state) -> state = On)
    |> List.length
    |> (printfn "Lights on: %d")