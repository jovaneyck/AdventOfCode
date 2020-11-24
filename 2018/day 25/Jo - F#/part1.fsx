#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
open System.IO
let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Seq.toList

type Point = { x: int ; y : int; z : int; t : int }
let point x y z t = {x=x; y=y; z=z; t=t}

type Constellation = Point Set
let singleton p = Set.singleton p

let parse (line : string) = 
    let vs = line.Split(',') |> Array.map int
    point vs.[0] vs.[1] vs.[2] vs.[3]

let distance one other =
          (abs (one.x - other.x))
        + (abs (one.y - other.y)) 
        + (abs (one.z - other.z)) 
        + (abs (one.t - other.t))
   
let inRangeOf one other =
    one
    |> Set.exists (fun p -> other |> Set.exists (fun pp -> distance p pp <= 3))

let merge (constellations : Constellation Set) (constellation : Constellation) =
    let constellationInRange = constellations |> Seq.tryFind (inRangeOf constellation)
    match constellationInRange with
    | None -> Set.add constellation constellations
    | Some constellationInRange -> 
        Set.union (Set.singleton (Set.union constellation constellationInRange)) (Set.difference constellations (Set.singleton constellationInRange))

let solve input =
    let rec fp f x =
        let y = f x
        if x = y then y else fp f y

    let singletonConstellations = 
        input
        |> Seq.toList
        |> List.map parse
        |> List.map singleton
        |> Set.ofList
    fp  (Set.fold merge Set.empty) singletonConstellations
    |> Seq.length
    
let tt () = 
    let example = @"0,0,0,0
                     3,0,0,0
                     0,3,0,0
                     0,0,3,0
                     0,0,0,3
                     0,0,0,6
                     9,0,0,0
                    12,0,0,0".Trim().Split('\n')
    let four = @"-1,2,2,0
                    0,0,2,-2
                    0,0,0,-2
                    -1,2,0,0
                    -2,-2,-2,2
                    3,0,2,-1
                    -1,3,2,2
                    -1,0,-1,0
                    0,2,1,-2
                    3,0,0,0".Trim().Split('\n')
    let three = @"1,-1,0,1
                    2,0,-1,0
                    3,2,-1,0
                    0,0,3,1
                    0,0,-1,-1
                    2,3,-2,0
                    -2,2,0,0
                    2,-2,0,-1
                    1,-1,0,-1
                    3,2,0,2".Trim().Split('\n')
    let eight = @"1,-1,-1,-2
                    -2,-2,0,1
                    0,2,1,3
                    -2,3,-2,1
                    0,2,3,-2
                    -1,-1,1,-2
                    0,-2,-1,0
                    -2,2,3,-1
                    1,2,2,0
                    -1,-2,0,-2".Trim().Split('\n')

    test <@ parse "-2,-4,5,-5" = {x=(-2);y=(-4);z=5;t=(-5)} @>
    test <@ distance (point 0 0 0 0) (point 3 0 0 0) = 3 @>
    test <@ distance (point 3 0 0 0) (point 0 0 0 0) = 3 @>
    test <@ distance (point 0 0 0 0) (point 0 3 0 0) = 3 @>
    test <@ distance (point 0 0 0 3) (point 0 0 0 6) = 3 @>
    test <@ distance (point 0 0 0 6) (point 0 0 0 3) = 3 @>
    test <@ distance (point 3 0 0 0) (point 0 3 0 0) = 6 @>
    test <@ distance (point 0 3 0 0) (point 3 0 0 0) = 6 @>
    test <@ distance (point 9 0 0 0) (point 0 0 0 6) = 15 @>    
    test <@ distance (point 0 0 0 6) (point 9 0 0 0) = 15 @>
    test <@ distance (point 12 0 0 0) (point 9 0 0 0) = 3 @>
    test <@ distance (point 9 0 0 0) (point 12 0 0 0) = 3 @>

    test <@ solve example = 2 @>
    test <@ solve four = 4 @>
    test <@ solve three = 3 @>
    test <@ solve eight = 8 @>

tt ()

solve input