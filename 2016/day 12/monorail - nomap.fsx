open System
open System.Text.RegularExpressions

let testInput = "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a"

type RegisterName = string

type Sink =
    | RawValue of int
    | Register of RegisterName

//Main strategy: I invented new command add + rewrote the input assembunny program so it's waaay faster
type Command = 
    | NOP
    | Add of source : string * destination : string
    | Inc of register : string
    | Dec of register : string
    | Cpy of source : Sink * destination : RegisterName
    | Jnz of source : Sink * line : int

type State = {pointer : int; a : int; b : int; c : int; d : int} //Map just doesn't cut it performance-wise for part 2, let's hardcode the registries into the type!

let split (text : string) = text.Split('\n') |> List.ofArray
let parseInt char = Int32.Parse(char |> string)

let groupValueOf (m : Match) (group : string) = m.Groups.[group].Value.Trim()
let buildIfMatch input regex builder =
    let m = Regex.Match(input, regex)
    if m.Success then
        Some (builder m)
    else
        None

let (|IsInc|_|) (text : string) =
    buildIfMatch
        text
        "inc (?<register>.*)$"
        (fun m -> (IsInc (groupValueOf m "register")))

let (|IsDec|_|) (text : string) =
    buildIfMatch
        text
        "dec (?<register>.*)$"
        (fun m -> (IsDec (groupValueOf m "register")))

let parseSink (sink : string) =
    match Int32.TryParse(sink) with
    | true, parsed -> RawValue parsed
    | false, _ -> Register sink
let (|IsCpy|_|) (text : string) =
    buildIfMatch
        text
        "cpy (?<src>.*) (?<dest>.*)$"
        (fun m -> 
            (IsCpy (groupValueOf m "src" |> parseSink, groupValueOf m "dest")))
let (|IsAdd|_|) (text : string) =
    buildIfMatch
        text
        "add (?<src>.*) (?<dest>.*)$"
        (fun m -> 
            (IsAdd (groupValueOf m "src", groupValueOf m "dest")))

let (|IsJnz|_|) (text : string) =
    buildIfMatch
        text
        "jnz (?<src>.*) (?<line>.*)$"
        (fun m -> 
            IsJnz (groupValueOf m "src" |> parseSink, groupValueOf m "line" |> parseInt))

let (|IsNOP|_|) (text : string) =
    buildIfMatch
        text
        "nop"
        (fun m -> IsNOP)

let parseLine line =
    match line with
    | IsInc reg -> Inc reg
    | IsDec reg -> Dec reg
    | IsCpy (src, dest) -> Cpy (src, dest)
    | IsJnz (src, line) -> Jnz (src, line)
    | IsAdd (src, dest) -> Add (src, dest)
    | IsNOP -> NOP
    | _ -> failwithf "Unknown command: %s" line

let parse input =
    input
    |> split
    |> List.map parseLine

let orDefault defaultValue value = defaultArg value defaultValue
let valueOf register state = 
    match register with
    | "a" -> state.a
    | "b" -> state.b
    | "c" -> state.c
    | "d" -> state.d
    | _ -> failwithf "Unknown register: %s" register
let update register value state = 
    match register with
    | "a" -> {state with a = value}
    | "b" -> {state with b = value}
    | "c" -> {state with c = value}
    | "d" -> {state with d = value}
    | _ -> failwithf "Unknown register: %s" register

let incrementPointer increment state =
    {state with pointer = state.pointer + increment}

let increment register (state : State) = 
    let value = valueOf register state
    let newValue = value + 1
    state 
    |> update register newValue
    |> incrementPointer 1

let decrement register (state : State) = 
    let value = state |> valueOf register
    let newValue = value - 1
    state
    |> update register newValue
    |> incrementPointer 1

let sinkValue state sink =
    match sink with
    | RawValue v -> v
    | Register r -> valueOf r state

let copy source destination state = 
    let value = sinkValue state source
    state
    |> update destination value
    |> incrementPointer 1

let jnz state source line =
    let value = sinkValue state source
    if value <> 0 then
        {state with pointer = state.pointer + line}
    else
        {state with pointer = state.pointer + 1}

let add src dest state =
    let srcValue = valueOf src state
    let destValue = valueOf dest state
    state
    |> update dest (srcValue + destValue)
    |> incrementPointer 1

let runInstruction state instruction = 
    match instruction with
    | Inc reg -> state |> increment reg
    | Dec reg -> state |> decrement reg
    | Cpy (src, dest) -> state |> copy src dest
    | Jnz (src, line) -> jnz state src line
    | NOP -> state |> incrementPointer 1
    | Add (src, dest) -> state |> add src dest

let rec run (state : State) program =
    let programLength = program |> List.length
    if (state.pointer > programLength - 1) then
        state
    else
        let instruction = program.[state.pointer]
        let newState = runInstruction state instruction
        run newState program

let initialState = {pointer = 0; a = 0; b = 0; c = 1; d = 0}

testInput
|> parse
|> run initialState

open System.IO
let input = File.ReadAllText(System.IO.Path.Combine([| __SOURCE_DIRECTORY__; "input.txt"|]))

#time
let endState =
    input
    |> parse
    |> run initialState