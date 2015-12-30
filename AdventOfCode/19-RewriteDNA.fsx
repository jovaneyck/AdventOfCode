﻿type Token = char list
type RewriteRule = {Src : Token; Dest : Token}

let seed = "e"
let medicine = "ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF"
let rulesInput = @"Al => ThF
Al => ThRnFAr
B => BCa
B => TiB
B => TiRnFAr
Ca => CaCa
Ca => PB
Ca => PRnFAr
Ca => SiRnFYFAr
Ca => SiRnMgAr
Ca => SiTh
F => CaF
F => PMg
F => SiAl
H => CRnAlAr
H => CRnFYFYFAr
H => CRnFYMgAr
H => CRnMgYFAr
H => HCa
H => NRnFYFAr
H => NRnMgAr
H => NTh
H => OB
H => ORnFAr
Mg => BF
Mg => TiMg
N => CRnFAr
N => HSi
O => CRnFYFAr
O => CRnMgAr
O => HP
O => NRnFAr
O => OTi
P => CaP
P => PTi
P => SiRnFAr
Si => CaSi
Th => ThCa
Ti => BP
Ti => TiTi
e => HF
e => NAl
e => OMg"


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

let rec (|StartsWith|_|) (pattern, text) = 
    match (pattern, text) with
    | ([], t) -> Some (StartsWith t)
    | (ph :: _, th :: _) when ph <> th -> None
    | (ph :: pt, th :: tt) when ph = th ->
        match (pt, tt) with
        | StartsWith remainder -> Some remainder
        | _ -> None
    | what -> failwith (sprintf "Was not expecting %A in this context!" what)

let rec allRewrites (text : char list) ({Src = s; Dest = d} as rule : RewriteRule) = 
    match (s, text) with
    | (_, []) -> [[]] 
    | StartsWith remainder -> (d @ remainder) :: ((allRewrites remainder rule) |> List.map(fun l -> s @ l))
    | (_, h :: t)  -> (allRewrites t rule) |> List.map (fun l -> h :: l)

let parseText (text : string) = text.ToCharArray() |> List.ofArray

let toString (chars : char list) : string = chars |> System.String.Concat

let step input rules =
    rules
    |> List.collect (allRewrites input)
    |> List.except [input]
    |> List.distinct

let rewritesFor rawRules rawText =
    let parsedInput = rawText |> parseText
    rawRules
    |> parse
    |> step parsedInput
    |> List.map toString

let parsedInput = seed |> parseText
let parsedRules = rulesInput |> parse

let recStep inp = 
    inp |> List.collect (fun i -> step i parsedRules)

let parsedMedicine = medicine |> parseText
let medicineFound (strings : char list list) =
    strings
    |> List.contains parsedMedicine

let rec medicineFoundInGeneration g input =
    if medicineFound input then
        g
    else
        printfn "Processing generation %i. Currently exploring %i paths." g (input |> List.length)
        medicineFoundInGeneration (g+1) (recStep input)

//let generation = medicineFoundInGeneration 0 [parsedInput]