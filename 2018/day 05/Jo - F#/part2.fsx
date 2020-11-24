let rec react (polymer : string) = 
    let isMatch a b = (string a).ToLower() = (string b).ToLower() && a <> b
    let toReplace =
        polymer
        |> Seq.pairwise
        |> Seq.tryFind (fun (a, b) -> isMatch a b) //destroying pairs one by one, doing them all in one pass could result in problems with e.g. "aAa"
    match toReplace with
    | None -> polymer
    | Some (a,b) ->
        let toDestroy = (string a) + (string b)
        let replaced = polymer.Replace(toDestroy, "")
        react replaced

let strip unit (polymer : string) =
    polymer.Replace(unit, "").Replace(unit.ToUpper(), "")

let findOptimalPolymer polymer =
    ['a'..'z']
    |> List.map string
    |> List.map (fun u -> (u, strip u polymer))
    |> List.map (fun (u, stripped) -> (u, react stripped))
    |> List.sortBy (fun (_, reacted) -> reacted |> Seq.length)
    |> List.head
    |> (fun (u,reacted) -> (u,reacted, reacted |> Seq.length))

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

printf "Testing..."
printfn "..done!"

test <@ ("c", "daDA", 4) = findOptimalPolymer "dabAcCaCBAcCcaDA" @>

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt") 
findOptimalPolymer input