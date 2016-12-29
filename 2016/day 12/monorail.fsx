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


type Command = 
    | Inc of register : string
    | Dec of register : string
    | Cpy of source : Sink * destination : RegisterName
    | Jnz of source : Sink * line : int

type State = {pointer : int; registers : Map<string, int>}

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

let (|IsJnz|_|) (text : string) =
    buildIfMatch
        text
        "jnz (?<src>.*) (?<line>.*)$"
        (fun m -> 
            IsJnz (groupValueOf m "src" |> parseSink, groupValueOf m "line" |> parseInt))

let parseLine line =
    match line with
    | IsInc reg -> Inc reg
    | IsDec reg -> Dec reg
    | IsCpy (src, dest) -> Cpy (src, dest)
    | IsJnz (src, line) -> Jnz (src, line)
    | _ -> failwithf "Unknown command: %s" line

let parse input =
    input
    |> split
    |> List.map parseLine

let increment register registers = 
    let value = registers |> Map.find register
    let newValue = value + 1
    registers
    |> Map.add register newValue

let orDefault defaultValue value = defaultArg value defaultValue
let valueOf register registers = registers |> Map.tryFind register |> orDefault 0
let update register value registers = registers |> Map.add register value

let decrement register registers = 
    let value = registers |> valueOf register
    let newValue = value - 1
    registers
    |> update register newValue

let sinkValue registers sink =
    match sink with
    | RawValue v -> v
    | Register r -> registers |> valueOf r

let copy source destination registers = 
    let value = sinkValue registers source
    registers
    |> update destination value

let jnz state source line =
    let value = sinkValue state.registers source
    if value <> 0 then
        {state with pointer = state.pointer + line}
    else
        {state with pointer = state.pointer + 1}

let runInstruction state instruction = 
    match instruction with
    | Inc reg -> {state with pointer = state.pointer + 1; registers = state.registers |> increment reg }
    | Dec reg -> {state with pointer = state.pointer + 1; registers = state.registers |> decrement reg }
    | Cpy (src, dest) -> {state with pointer = state.pointer + 1; registers = state.registers |> copy src dest }
    | Jnz (src, line) -> jnz state src line

let rec run (state : State) program =
    let programLength = program |> List.length
    if (state.pointer > programLength - 1) then
        state
    else
        let instruction = program.[state.pointer]
        printfn "Running %A on state %A" instruction state
        let newState = runInstruction state instruction
        run newState program

let initialState = {pointer = 0; registers = Map.empty}

testInput
|> parse
|> run initialState

open System.IO
let input = File.ReadAllText(System.IO.Path.Combine([| __SOURCE_DIRECTORY__; "input.txt"|]))

let endState =
    input
    |> parse
    |> run {pointer = 0; registers = [("c", 1)] |> Map.ofList}