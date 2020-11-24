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

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

printf "Testing..."
test <@ "dabCBAcaDA" = react "dabAcCaCBAcCcaDA" @>
printfn "..done!"



let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt") 
let reduced = reduce input
Seq.length reduced