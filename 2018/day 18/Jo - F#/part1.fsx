#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type AreaKind = Open | Trees | Lumberyard
type Grid = Map<int*int, AreaKind>

let parse input =
    let toArea parsed : Grid =
        parsed
        |> List.mapi (fun y row -> row |> List.mapi (fun x kind -> (x,y),kind))
        |> List.collect id
        |> Map.ofList
    let parseArea =
        function
        | '.' -> Open
        | '|' -> Trees
        | '#' -> Lumberyard
    input
    |> List.map (Seq.toList >> List.map parseArea)
    |> toArea

let neighbours grid location kind = 
    let deltas =
        [ (-1,-1); (0, -1); (1, -1);
            (-1,0);           (1,0);
            (-1, 1); (0,1);   (1,1)]
    let neighbours =
        deltas
        |> List.map (fun (x,y) -> (x + fst location, y + snd location))
    let ofKind =
        neighbours
        |> List.choose (fun n -> grid |> Map.tryFind n)
        |> List.filter (function | k when k = kind -> true | _ -> false)
    ofKind |> List.length


let next grid (location,kind) = 
    let next =
        match kind with
        | Open when neighbours grid location Trees >= 3 -> Trees
        | Open -> Open
        | Trees when neighbours grid location Lumberyard >= 3 -> Lumberyard
        | Trees -> Trees
        | Lumberyard when (neighbours grid location Lumberyard >= 1) && (neighbours grid location Trees >= 1) -> Lumberyard
        | Lumberyard -> Open
    (location, next)

let tick grid = 
    grid 
    |> Map.toList
    |> List.map (fun area -> next grid area)
    |> Map.ofList

let print grid = 
    let l = grid |> Map.toList
    let xmax = l |> List.map (fst >> fst) |> List.max
    let ymax = l |> List.map (fst >> snd) |> List.max
    [ for y in 0 .. ymax do
        for x in 0 .. xmax ->
            let kind = grid |> Map.find (x,y)
            match kind with
            | Open -> "."
            | Trees -> "|"
            | Lumberyard -> "#"
        yield "\n"]
    |> String.concat ""
    |> printfn "%s"


let example = [
    ".#.#...|#."
    ".....#|##|"
    ".|..|...#."
    "..|#.....#"
    "#.#|||#|#|"
    "...#.||..."
    ".|....|..."
    "||...#|.#|"
    "|.||||..|."
    "...#.|..|."]

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> List.ofSeq

let rec loop nb grid : unit =
    let next = tick grid
    //if nb % 100 = 0 then 
    let kinds = next |> Map.toList |> List.map snd 
    let trees = kinds |> List.filter (function | Trees -> true | _ -> false) |> Seq.length
    let lumberyards = kinds |> List.filter (function | Lumberyard -> true | _ -> false) |> Seq.length
    let result = trees * lumberyards
    printfn "Generation %d: (%d, %d, %d)" nb trees lumberyards result

    loop (nb+1) next

let parsed = input |> parse
//1_000_000_000
loop 0 parsed