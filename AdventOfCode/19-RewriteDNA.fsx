type Token = char list
type RewriteRule = {Src : Token; Dest : Token}

let rulesInput = @"H => HO
H => OH
O => HH"
let textInput = "HOH"

let parseToken (token : string) = 
    token.ToCharArray() 
    |> List.ofArray

let parseLine (l : string) =
    let tokens = l.Split([|" => "|], System.StringSplitOptions.RemoveEmptyEntries)
    { Src = tokens.[0] |> parseToken; Dest = tokens.[1] |> parseToken}

let parse (input : string) =
    input.Split('\n')
    |> List.ofArray
    |> List.map parseLine

let allRewrites text rule = [text; text]

let parseText (text : string) = text.ToCharArray() |> List.ofArray

let toString (chars : char list) = chars |> Array.ofList |> System.String

let rewritesFor rawRules rawText =
    rawRules
    |> parse
    |> List.collect (allRewrites (rawText |> parseText)) 
    |> List.map toString
    |> List.distinct

rewritesFor rulesInput textInput