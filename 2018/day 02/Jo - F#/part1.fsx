#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let countLetters word = 
    Seq.groupBy id word 
    |> Seq.map (fun (letter, occ) -> (letter, occ |> Seq.length))

let contains nb counts =
    counts |> Seq.map snd |> Seq.exists ((=) nb)

let solve input = 
    let counts = input |> Seq.map countLetters
    let twos = counts |> Seq.filter (contains 2) |> Seq.length
    let threes = counts |> Seq.filter (contains 3) |> Seq.length
    twos * threes

let input = System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "\input.txt")
solve input
let example = 
    ["abcdef"
     "bababc"
     "abbcde"
     "abcccd"
     "aabcdd"
     "abcdee"
     "ababab"]

printf "Testing.."    
test <@ solve example = 12 @>
printfn "..done!"