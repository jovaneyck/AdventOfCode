#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
open System.Text.RegularExpressions
open System.IO

let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Seq.toList

type Location = { x : int64; y : int64; z : int64 }
type Radius = Radius of int64
type Nanobot = { location : Location; radius : Radius }

let example = @"pos=<0,0,0>, r=4
pos=<1,0,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<1,3,1>, r=1".Split('\n') |> Seq.toList

let parse nanobot = 
    let m = Regex.Match(nanobot, "pos=<(-?\d*),(-?\d*),(-?\d*)>, r=(\d*)")
    let int64At (pos : int) = m.Groups.[pos].Value |> int64
    { location = { x = int64At 1; y = int64At 2; z = int64At 3 }; radius = Radius <| int64At 4 }

let distance { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } = 
    [abs (x1 - x2); abs (y1 - y2); abs (z1 - z2)]
    |> List.sum
    |> abs

let inRangeOf bots ({ radius = Radius r} as bot) =
    bots
    |> List.filter (fun b -> distance bot.location b.location <= r)

let solve input =
    let bots = 
        input
        |> List.map parse
    let bestBot = bots |> List.maxBy (fun b -> b.radius)
    let inRange = bestBot |> inRangeOf bots
    inRange |> List.length

let tt () = 
    test <@ parse "pos=<2,3,4>, r=5" = { location = { x = 2L; y = 3L; z = 4L}; radius = Radius 5L } @>
    test <@ parse "pos=<-2,-3,-4>, r=5" = { location = { x = -2L; y = -3L; z = -4L}; radius = Radius 5L } @>
    test <@ distance { x = 0L; y = 0L; z = 0L } { x = 0L; y = 5L; z = 0L} = 5L @>
    test <@ distance { x = 0L; y = 0L; z = 0L } { x = 4L; y = 0L; z = 0L} = 4L @>
    test <@ distance { x = 4L; y = 0L; z = 0L} { x = 0L; y = 0L; z = 0L} = 4L @>
    test <@ distance { x = -3L; y = 0L; z = 0L} { x = -5L; y = 0L; z = 0L} = 2L @>
    test <@ solve example = 7 @>

tt ()

let part1 = solve input