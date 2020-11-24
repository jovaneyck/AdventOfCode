open System.Text.RegularExpressions

type Coords = { x : int; y : int}
type Dimensions = {w : int; h : int}
type Claim = { id : int; coords : Coords; dimensions: Dimensions }
let parse line =
    let parseInt = System.Int32.Parse
    let pattern = @"#(\d*) @ (\d*),(\d*): (\d*)x(\d*)"
    let m = Regex.Match(line, pattern)
    let get (i : int) = m.Groups.[i].Value

    { id =  get 1|> parseInt
      coords = { x = get 2 |> parseInt; y = get 3|> parseInt}
      dimensions = { w = get 4 |> parseInt; h = get 5 |> parseInt}
    }

let toPoints {coords = {x = x;y = y}; dimensions = {w = w; h = h} } =
    [ for ww in [0..w-1] do
      for hh in [0..h-1] ->
        (x + ww, y + hh) ]

let overlaps r1 r2 =
    not <| (r1.coords.x + r1.dimensions.w < r2.coords.x
            || r1.coords.y + r1.dimensions.h < r2.coords.y
            || r1.coords.x > r2.coords.x + r2.dimensions.w
            || r1.coords.y > r2.coords.y + r2.dimensions.h);

let overlap rect1 rect2 =
    if not <| overlaps rect1 rect2 then Set.empty
    else
        Set.intersect
            (Set.ofList <| toPoints rect1)
            (Set.ofList <| toPoints rect2)

let getOverlaps claims claim =
    //printfn "Getting overlaps for %d" claim.id
    claims
    |> List.fold (fun acc c -> Set.union (overlap claim c) acc ) Set.empty

let solve input =
    let claims = input |> List.map parse
    claims 
    |> List.map (fun claim -> (claim, (getOverlaps (claims |> List.except [claim]) claim)))
    |> List.choose (fun (claim, overlaps) -> if overlaps |> Seq.isEmpty then Some claim else None)
    |> List.head
    |> (fun c -> c.id)

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let example = [ "#1 @ 1,3: 4x4"
                "#2 @ 3,1: 4x4"
                "#3 @ 5,5: 2x2"]

test <@ solve example = 3 @>

let input = 
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "\input.txt") 
    |> Seq.toList

solve input