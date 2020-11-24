let nbDifferences (dictionary : string list) (word : string) =
    let rec countDifferences acc one other =
        match one, other with
        | [], [] -> acc
        | x::xs, y::ys when x = y -> countDifferences acc xs ys
        | _::xs, _::ys -> countDifferences (acc + 1) xs ys

    dictionary 
    |> List.map (fun w -> w, countDifferences 0 (w |> Seq.toList) (word |> Seq.toList))

let diff nb (_, counts) = 
    let withNb = counts |> Seq.filter (fun (_,c) -> c = nb)
    if Seq.length withNb = 1
    then withNb |> Seq.head |> fst |> Some
    else None

let solve input =
    input 
    |> Seq.map (fun word -> word, nbDifferences (input |> List.except (Seq.singleton word)) word)
    |> Seq.choose (diff 1)
    |> Seq.toList

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let example = 
       ["abcde"
        "fghij"
        "klmno"
        "pqrst"
        "fguij"
        "axcye"
        "wvxyz"]

printf "Testing.."
test <@ nbDifferences ["abc";"abd"] "abc" = [("abc", 0); ("abd", 1)] @>
test <@ solve example = ["fguij"; "fghij"]  @>
printfn "..done!"

let input = System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "\input.txt")
solve (input |> Seq.toList)

//["wlkiwgsqyfecjqqmnxaktdrhbz"; 
// "wlkiogsqyfecjqqmnxaktdrhbz"]
//  wlkigsqyfecjqqmnxaktdrhbz