#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type RegisterName = RegisterName of char
type Registers = Map<RegisterName, int64>
type Value = | FromRegister of RegisterName | RawValue of int64
let registerName = RegisterName
let registerValue r = FromRegister <| RegisterName r
let integerValue i = RawValue i

type Instruction = 
    | Set of (RegisterName * Value)
    | Sub of (RegisterName * Value)
    | Mul of (RegisterName * Value)
    | Jnz of (Value * Value)

type State = { Pointer : int64; Registers : Registers; Multiplications : int }
let init = { Pointer = 0L; Registers = Map.empty; Multiplications = 0 }

let split (delims : string array) (text : string) = 
    text.Split(delims, System.StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray

let tryParseInt text = 
    text
    |> Seq.map string
    |> String.concat ""
    |> System.Int64.TryParse

let parseValue (text : char list) = 
    match tryParseInt text with
    | true, parsed -> integerValue parsed
    | _ -> registerValue (char text.[0])

let parseInstruction text =
    match text |> Seq.toList with
    | 's'::'e'::'t' :: ' ' :: reg :: ' ' :: v -> Set <| (registerName reg, parseValue v)
    | 's'::'u'::'b' :: ' ' :: reg :: ' ' :: v -> Sub <| (registerName reg, parseValue v)
    | 'm'::'u'::'l' :: ' ' :: reg :: ' ' :: v -> Mul <| (registerName reg, parseValue v)
    | 'j'::'n'::'z' :: ' ' :: v1 :: ' ' :: v2 -> Jnz <| (parseValue [v1] (*OUCH, incorrect when multi-digit*), parseValue v2)
    | unknown -> failwithf "Failed to parse %A" unknown

let parse instructions =
    instructions
    |> split [|"\n"; "\r"|]
    |> List.map parseInstruction

let getRegisterValue register (registers : Registers) =
    registers
    |> Map.tryFind register
    |> (fun o -> defaultArg o 0L)

let getValue v registers = 
    match v with
    | FromRegister registerName -> getRegisterValue registerName registers
    | RawValue rawValue -> rawValue

let setValue register v state = 
    let newRegisters =
        state.Registers 
        |> Map.add register v
    { state with Registers = newRegisters }

let incrementPointer increment state = 
    {state with Pointer = state.Pointer + increment}

let set reg value state = 
    let v = getValue value state.Registers
    state
    |> setValue reg v
    |> incrementPointer 1L

let incrementMultiplications state = 
    { state with Multiplications = state.Multiplications + 1 }

let mul reg value state = 
    let oldValue = getRegisterValue reg state.Registers
    let v = getValue value state.Registers
    let newValue = oldValue * v
    state 
    |> setValue reg newValue
    |> incrementPointer 1L
    |> incrementMultiplications

let sub reg value state = 
    let oldValue = getRegisterValue reg state.Registers
    let v = getValue value state.Registers
    let newValue = oldValue - v
    state 
    |> setValue reg newValue
    |> incrementPointer 1L

let jnz cond offset state =
    let v = getValue cond state.Registers
    if v <> 0L then
        let offsetV = getValue offset state.Registers
        state
        |> incrementPointer offsetV
    else
        state
        |> incrementPointer 1L

let runInstruction instruction state = 
    match instruction with
    | Set (reg, value) -> set reg value state
    | Sub (reg, value) -> sub reg value state
    | Mul (reg, value) -> mul reg value state
    | Jnz (cond, offset) -> jnz cond offset state

let print state =
    let registers =
        state.Registers
        |> Map.toList
        |> List.map (fun (RegisterName r,v) -> sprintf "(%A,%d)" r v)
        |> String.concat ";"
    sprintf 
        "Pointer: %d, Registers: %s" 
        state.Pointer 
        registers

let rec run instructions state =
    let l = instructions |> Seq.length |> int64
    if state.Pointer < 0L || state.Pointer >= l then
        state
    else
        let i = instructions |> List.item (int state.Pointer)
        //printfn "Executing %A on %s" i (print state)
        let newState = runInstruction i state
        run instructions newState

let input = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt" )
let instructions = input |> parse
let part1 = run instructions init