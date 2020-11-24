#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote  

let parse lines = 
    seq {
        for y,row in (lines |> Seq.indexed) do
        for x,cell in (row |> Seq.indexed) do
        if cell = '#' then yield (x,y)
    }
    |> List.ofSeq

let distance (x1,y1) (x2,y2) = 
    pown (float x2 - float x1) 2 + pown (float y2 - float y1) 2
    |> sqrt

(* Calculates the angle between the given vector and (0,-infinity) (upward y axis, where the laser starts) *)
let angle (x1,y1) (x2,y2) =
    let (dx,dy) = (x2 - x1, y2 - y1)
    atan2 (float dx) (float dy) - atan2 (float x1) (float System.Int32.MinValue)

(* (-1,-1) (0,-1) (1,-1)
    (-1,0) (0, 0) (1,0)
    (-1,1) (0, 1) (1,1)
sorted: (0,-1);(1,-1);(1,0);(1,1);(0,1);(-1,1);(-1,0)
*)
test <@ 
        let sorted = [(0,-1);(1,-1);(1,0);(1,1);(0,1);(-1,1);(-1,0)] |> List.sortByDescending (angle (0,0))
        sorted = [(0,-1);(1,-1);(1,0);(1,1);(0,1);(-1,1);(-1,0)] @>


let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> List.ofSeq
//let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\example.txt") |> List.ofSeq
//let input = [
//                ".#....#####...#.."
//                "##...##.#####..##"
//                "##...#...#.#####."
//                "..#.....#...###.."
//                "..#.#.....#....##" ]
let baseA = (19, 14)
//let baseA = (11,13)

let asteroids = parse input

let baseView =
    asteroids 
    |> List.map (fun a -> a, asteroids |> List.except [a] |> List.groupBy (angle a) ) 
    |> List.find (fun (a,_) -> a = baseA) 
    |> snd
    |> List.sortByDescending (fun (a, _) -> a)
    |> List.map (fun (angle, asts) -> (angle, asts |> List.sortBy (distance baseA) ) )
    |> List.map snd

let rec destructionSequence asts =
    match asts with
    | [] -> []
    | h :: t ->
        match h with
        | next :: hs -> next :: (destructionSequence (List.append t [hs]))
        | [] -> destructionSequence t

let result = destructionSequence baseView |> List.indexed
result |> Seq.iter (printfn "%A")