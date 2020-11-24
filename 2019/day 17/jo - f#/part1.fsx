#load "IntCode.fsx"
open IntCode.IntCode

//IntCode.t ()

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : IntCode.Program = input.Split([|','|]) |> Seq.map int64 |> Seq.toList
let initProg = (IntCode.initState [] p)

let loaded = initProg |> IntCode.run

let split maze =
    let rec split maze =
        match maze with
        | [] -> []
        | '\010' :: ms -> [] :: (split ms)
        | c :: ms ->
            let split_h :: split_ms = split ms
            (c :: split_h) :: split_ms
    split maze |> List.rev |> List.skip 1 |> List.rev

let toString (maze : char list list) = 
    [
        for row in maze do
            for cell in row do
                yield cell |> string
            yield "\n"
    ] |> String.concat ""

let toMap (maze : char list list) =
    [
        for (y,row) in maze |> List.indexed do
            for (x,cell) in (row |> List.indexed) do
                yield (x,y),cell
    ]
    |> Map.ofList

let neighbours map (x,y) =
    let deltas = [(-1,0);(1,0);(0,-1);(0,1)]
    deltas |> List.map (fun (dx,dy) -> (x+dx,y+dy))  |> List.choose (fun d -> map |> Map.tryFind d)

let rawMaze = loaded.output |> List.rev |> List.map (int >> char)
let maze = rawMaze |> split 
toString maze
let indexed = toMap maze
let walls = indexed |> Map.toList |> List.filter (fun (_,c) -> c = '#')
let intersections = walls |> List.filter (fun (wloc,_) -> neighbours (walls |> Map.ofList) wloc |> List.length = 4)
let params = intersections |> List.map fst |> List.map (fun (x,y) -> x * y) |> List.sum