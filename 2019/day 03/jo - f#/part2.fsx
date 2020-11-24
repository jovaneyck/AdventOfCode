#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Direction =
    | U
    | D
    | L
    | R
type Section = { direction : Direction; steps : int }
type Location = int * int
type Path = (Location * int) list

let parse (section : string) =
    let direction = section.Chars 0
    let stepsize = section.Substring 1 |> int
    let d =
        match direction with
        | 'U' -> U
        | 'D' -> D
        | 'L' -> L
        | 'R' -> R
        | err -> failwithf "Unknown direction: %c" err
    { direction = d; steps = stepsize }

let step (x,y) direction =
    match direction with
    | U -> (x, y + 1)
    | D -> (x, y - 1)
    | L -> (x - 1, y)
    | R -> (x + 1, y)

let unfoldSection pathLength location section =
    List.unfold 
        (fun (steps, loc, pathLength) -> 
            if steps = 0 then None
            else 
                let next = step loc section.direction
                Some ((next, pathLength), (steps - 1, next, pathLength + 1))) 
        (section.steps, location, pathLength)

type State = { steps : int; location : Location; path : Path }
let path state (section : Section) =
    let unfolded = unfoldSection state.steps state.location section
    { state with
        steps = state.steps + section.steps
        location = unfolded |> List.last |> fst
        path = List.append state.path unfolded }

let pathFor sections =
    sections
    |> Seq.fold path { location = (0,0); path = []; steps = 1 }
    |> (fun s -> s.path)

let intersections paths =
    Set.intersectMany (paths |> Seq.map Set.ofList)
    |> Set.toList

let result (input : string list) =
    let parsedWires = input |> List.map (fun wire -> wire.Split([|','|]) |> Seq.toList |> List.map parse)

    let paths =
        parsedWires 
        |> List.map pathFor     

    let intersects = 
        paths 
        |> List.map (fun p -> p |> List.map fst)
        |> intersections

    let p1 = paths.[0]
    let p2 = paths.[1]

    intersects 
    |> List.map (fun i-> 
                    let (_,s1) = p1 |> List.find (fun (l,_) -> l = i)
                    let (_,s2) = p2 |> List.find (fun (l,_) -> l = i)
                    s1+s2)
    |> List.min

printf "Testing..."

test <@ parse "R1005" = { direction = R; steps = 1005 } @>
test <@ pathFor [ {direction = U; steps = 1} ; {direction=R;steps= 2} ] = [((0, 1), 1); ((1, 1), 2); ((2, 1), 3)]  @>
test <@ intersections [[(1,1); (1,2); (1,3)]; [(0,1);(1,3);(1,2)]] = [(1,2);(1,3)] @>

test <@ result ["R8,U5,L5,D3"; "U7,R6,D4,L4"] = 30 @>

printfn "..done!"

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Seq.toList

result input