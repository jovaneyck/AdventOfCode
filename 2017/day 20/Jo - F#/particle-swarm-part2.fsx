#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
open System

//Strategy: fixpoint while removing collisions from the particle set between ticks

type Vector = { X : int; Y : int; Z : int }
type Particle = {Id : int; Position : Vector; Velocity : Vector; Acceleration : Vector}

let zero = {X = 0; Y = 0; Z = 0}

let splitLines (text : string) = 
    List.ofArray <| text.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)

let parseInt = System.Int32.Parse

let parseParticle id text = 
    let m = 
        System.Text.RegularExpressions.Regex(
            "p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>").Match(text)
    let valueAt (index : int) = m.Groups.[index].Value
    let intValueAt index = parseInt <| valueAt index
    let (px,py,pz) = (intValueAt 1, intValueAt 2, intValueAt 3)
    let (vx,vy,vz) = (intValueAt 4, intValueAt 5, intValueAt 6)
    let (ax,ay,az) = (intValueAt 7, intValueAt 8, intValueAt 9)
    { Id = id; Position = {X = px; Y = py; Z = pz}; Velocity = {X = vx; Y = vy; Z = vz}; Acceleration = {X = ax; Y = ay; Z = az}}

let parse text = 
    text
    |> splitLines
    |> List.indexed
    |> List.map (fun (id, line) -> parseParticle id line)

let add v1 v2 =
    {X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z}

let increaseVelocity ({Velocity = v; Acceleration = a} as p) =
    { p with Velocity = add v a}

let increasePosition ({Position = pos; Velocity = v} as p) = 
    { p with Position = add pos v }

let tickParticle p = 
    p
    |> increaseVelocity
    |> increasePosition

let tick particles =
    List.map tickParticle particles

let rec repeat n f x =
    if n = 0 then x
    else repeat (n-1) f (f x)

let length vector =
    [vector.X; vector.Y; vector.Z] |> List.sumBy abs

let closestToOriginPerTick particles = 
    let next = tick particles
    let closest = next |> List.minBy (fun p -> length p.Position)
    Some (closest.Id, particles)

let part1 nb particles = 
    Seq.unfold closestToOriginPerTick particles
    |> Seq.take nb
    |> Seq.last

let filterCollisions particles =
    let locations = particles |> List.map (fun p -> p.Position)
    let collisionLocations = locations |> List.groupBy id |> List.filter (fun (_,hits) -> List.length hits > 1) |> List.map fst
    particles |> List.filter (fun p -> collisionLocations |> List.contains p.Position |> not)

let part2 nb particles =
    repeat nb (tick >> filterCollisions) particles 
    |> List.length

let example = "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>\np=<-4,0,0>, v=<2,0,0>, a=<0,0,0>\np=<-2,0,0>, v=<1,0,0>, a=<0,0,0>\np=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"
part2 2 (parse example)

printf "Testing..."
test <@ parse "p=<1199,-2918,1457>, v=<-13,115,-8>, a=<-7,8,-10>" 
            = [{Id =0; Position = {X = 1199; Y = -2918; Z = 1457}; Velocity = {X = -13; Y = 115; Z = -8}; Acceleration = {X = -7; Y = 8; Z = -10}}] @>
test <@ parse "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>" |> repeat 3 tick 
            = [{Id = 0; Position = {X = 3;Y = 0;Z = 0;};Velocity = {X = -1;Y = 0;Z = 0;};Acceleration = {X = -1;Y = 0;Z = 0;};}] @>
test <@ parse "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>" |> repeat 3 tick 
            = [{Id = 0; Position = {X = -8;Y = 0;Z = 0;};Velocity = {X = -6;Y = 0;Z = 0;};Acceleration = {X = -2;Y = 0;Z = 0;};}] @>
test <@ length { X = 1; Y = 2; Z = -3 } = 6 @>
test <@ part1 4 (parse example) = 0 @>
printfn "..done!"

let input = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt" )
let parsed = parse input
part2 50 parsed //"fix point" as in trial & error until response stays fixed for a bit :)