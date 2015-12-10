module Circuits

open System

type WireId = string
type Signal = uint16
type Wire = { Id : WireId; Signal: Signal }
type ShiftAmount = uint16
type SignalSource =
    | ConstantValue of Signal
    | OtherWire of WireId
    | NOT of SignalSource
    | AND of SignalSource * SignalSource
    | OR of SignalSource * SignalSource
    | LSHIFT of SignalSource * ShiftAmount
    | RSHIFT of SignalSource * ShiftAmount
type Instruction = { Source : SignalSource; Wire : WireId }

let interpretSource (source : string) : SignalSource = 
    match UInt16.TryParse(source) with
    | (true, value) -> ConstantValue value
    | (false, _) -> OtherWire source

let interpretLine tokens = 
    match tokens with
    | "NOT" :: source :: "->" :: identifier :: [] -> {Source = NOT (interpretSource source); Wire = identifier}
    | srcA :: "AND" :: srcB :: "->" :: identifier :: [] -> {Source = AND ((interpretSource srcA), (interpretSource srcB)); Wire = identifier}
    | srcA :: "OR" :: srcB :: "->" :: identifier :: [] -> {Source = OR ((interpretSource srcA), (interpretSource srcB)); Wire = identifier}
    | source :: "LSHIFT" :: amount :: "->" :: identifier :: [] -> {Source = LSHIFT ((interpretSource source), UInt16.Parse(amount)); Wire = identifier}
    | source :: "RSHIFT" :: amount :: "->" :: identifier :: [] -> {Source = RSHIFT ((interpretSource source), UInt16.Parse(amount)); Wire = identifier}
    | sourceId :: "->" :: identifier :: [] -> {Source = (interpretSource sourceId); Wire = identifier}
    | unrecognized -> failwith (sprintf "Unrecognized command: %A" unrecognized)

let parseLine (line : string) =
    line.Split(' ') 
    |> List.ofArray
    |> interpretLine

let parse (inputText : string) = 
    inputText.Split('\n')
    |> List.ofArray
    |> List.map parseLine

let anded x y = x &&& y
let ored x y = x ||| y
let lshifted s amount = s <<< 2
let rshifted s amount = s >>> 2
let noted s = ~~~s

let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None ->
             let res = f x
             cache := (!cache).Add(x,res)
             res

let rec calculate = memoize (fun parsedInstructions (source : SignalSource) ->
    match source with
    | ConstantValue v -> v
    | OtherWire o -> calculate parsedInstructions (parsedInstructions |> List.find (fun {Wire = id} -> id = o)).Source
    | NOT o -> noted (calculate parsedInstructions o)
    | LSHIFT (o, amount) -> lshifted (calculate parsedInstructions o) amount
    | RSHIFT (o, amount) -> rshifted (calculate parsedInstructions o) amount
    | AND (x,y) -> anded (calculate parsedInstructions x) (calculate parsedInstructions y)
    | OR (x,y) -> ored (calculate parsedInstructions x) (calculate parsedInstructions y))


let calculateAllSignalsFor instrs = 
    let parsed = instrs |> parse
    parsed
    |> List.map (fun i -> (i.Wire, calculate parsed (OtherWire i.Wire)))