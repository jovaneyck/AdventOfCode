#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Orbit = { center : string; orbit : string }
let parse (orbit : string) = 
    let c::o::_ = orbit.Split([|')'|]) |> List.ofArray
    { orbit = o; center = c }

let orbitingObjects = 
    List.map (fun o -> o.orbit)
    >> List.distinct

let rec orbitsOf orbits object =
    let direct = orbits |> List.tryFind (fun o -> o.orbit = object)
    match direct with
    | None -> []
    | Some o -> o :: orbitsOf orbits o.center 

let example = 
    "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN".Split([|'\n'|]) 
    |> Seq.map parse
    |> Seq.toList

let rec closestCommon s y =
    let myPosition :: ys = y
    let santaOrbits = s |> List.tryFind (fun o -> o.center = myPosition.center)
    match santaOrbits with
    | None -> closestCommon s ys
    | Some o -> o

let distance orbits o =
    let rec d orbits acc =
        let curr :: os = orbits
        if curr.center = o.center then acc
        else d os (acc + 1)
    d orbits 0

let solve orbits =
    let s = orbitsOf orbits "SAN"
    let y = orbitsOf orbits "YOU"
    let common = closestCommon s y
    let d1 = distance y common
    let d2 = distance s common
    d1 + d2

printf "Testing..."

test <@ parse "AAA)BBB" = { center = "AAA" ; orbit = "BBB" } @>
test <@ ["B";"C";"D"] = orbitingObjects [ { center = "A"; orbit = "B" } ; { center = "B"; orbit = "C" } ; {center = "A"; orbit = "D"} ] @>
test <@ orbitsOf [ { center = "A"; orbit = "B" } ; { center = "B"; orbit = "C" } ] "C" |> Seq.length = 2 @>
test <@ solve example = 4 @>
printfn "..done!"

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
input
|> Seq.map parse
|> Seq.toList
|> solve