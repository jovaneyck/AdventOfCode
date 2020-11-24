#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let filePath = sprintf "%s\%s" __SOURCE_DIRECTORY__ "passphrases.txt"
let phrases = System.IO.File.ReadAllLines(filePath)

let splitWords (phrase : string) = phrase.Split([|' '|])

let wordCounts phrase =
    phrase 
    |> Seq.countBy id

let noDoubles counts =
    counts 
    |> Seq.exists (fun (_,count) -> count <> 1) 
    |> not

let validPhrases phrases = 
    phrases
    |> Seq.map splitWords
    |> Seq.map wordCounts
    |> Seq.filter noDoubles
    |> Seq.length

validPhrases phrases

//part 2
let noAnagrams words =
    let letterCounts =
        words
        |> Seq.map (fun word -> word |> Seq.groupBy id |> Seq.map (fun (letter, occurrences) -> (letter, occurrences |> Seq.length)) |>Seq.sort)
    
    //Gotcha: seq equality does not seem to work? List gives no problem. RESEARCH!
    let l = letterCounts |> Seq.toList |> List.map (Seq.toList)
    l = (l |> List.distinct)

let validPhrases2 phrases = 
    let isValid words =
       words |> wordCounts |> noDoubles && noAnagrams words

    phrases
    |> Seq.map splitWords
    |> Seq.filter isValid
    |> Seq.length

validPhrases2 phrases

test <@ not <| noAnagrams [|"ab"; "ba" |] @>
test <@ noAnagrams [|"ab"; "bc"|] @>