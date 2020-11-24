#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

//Using cube coordinates as documented in https://www.redblobgames.com/grids/hexagons/

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

let distance (x,y,z) = 
    let abs (n : int) = System.Math.Abs n
    ([abs x ; abs y ; abs z] |> List.sum) / 2

type State = { location : int * int * int; farthest : int }

let run text =
    let parsed = 
        text
        |> split
        |> List.map parse

    let takeStep {location = location; farthest = farthest} step =
        let (x,y,z) = location

        let newLocation = 
            match step with
            | N -> (x,y+1, z-1)
            | NE -> (x+1,y,z-1)
            | SE -> (x+1,y-1,z)
            | S -> (x,y-1,z+1)
            | SW -> (x-1,y,z+1)
            | NW -> (x-1,y+1,z)

        let newFarthest = [farthest; distance newLocation] |> List.max

        {location = newLocation; farthest = newFarthest}

    let endLocation =
        parsed
        |> List.fold takeStep { location = (0,0,0); farthest = 0 }
    endLocation

let input = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt")
run input