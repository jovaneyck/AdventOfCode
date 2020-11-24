#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

//Using axial coordinates as documented in https://www.redblobgames.com/grids/hexagons/

type Step =
    | N
    | NE
    | SE
    | S
    | SW
    | NW

let split (text : string) = text.Split([|','|]) |> List.ofSeq

let parse =
    function
    | "n" -> N
    | "ne" -> NE
    | "se" -> SE
    | "s" -> S
    | "sw" -> SW
    | "nw" -> NW
    | _ -> failwith "Illegal direction"

let run text =
    let parsed = 
        text
        |> split
        |> List.map parse

    let takeStep location =
        let (x,y,z) = location
        function
        | N -> (x,y+1, z-1)
        | NE -> (x+1,y,z-1)
        | SE -> (x+1,y-1,z)
        | S -> (x,y-1,z+1)
        | SW -> (x-1,y,z+1)
        | NW -> (x-1,y+1,z)

    let endLocation =
        parsed
        |> List.fold takeStep (0,0,0)
    endLocation

let distance (x,y,z) = 
    let abs (n : int) = System.Math.Abs n
    ([abs x ; abs y ; abs z] |> List.sum) / 2

test <@ run "n" = (0,1,-1) @>
test <@ run "ne" = (1,0,-1) @>
test <@ run "se" = (1,-1,0) @>
test <@ run "sw" = (-1,0,1) @>
test <@ run "nw" = (-1,1,0) @>

test <@ run "n,se,s,sw,nw,n,ne,s" = (0,0,0) @> //full circle

let input = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt")
run input |> distance