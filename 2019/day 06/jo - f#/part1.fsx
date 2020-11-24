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

let solve orbits =
    orbits
    |> orbitingObjects
    |> List.map (orbitsOf orbits >> Seq.length)
    |> List.sum


let example = 
    "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L".Split([|'\n'|]) 
    |> Seq.map parse
    |> Seq.toList

printf "Testing..."

test <@ parse "AAA)BBB" = { center = "AAA" ; orbit = "BBB" } @>
test <@ ["B";"C";"D"] = orbitingObjects [ { center = "A"; orbit = "B" } ; { center = "B"; orbit = "C" } ; {center = "A"; orbit = "D"} ] @>
test <@ orbitsOf [ { center = "A"; orbit = "B" } ; { center = "B"; orbit = "C" } ] "C" |> Seq.length = 2 @>
test <@ solve example = 42 @>

printfn "..done!"

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
input
|> Seq.map parse
|> Seq.toList
|> solve