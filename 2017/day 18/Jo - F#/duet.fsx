#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type RegisterName = RegisterName of char
type Registers = Map<RegisterName, int64>
type Value = | FromRegister of RegisterName | RawValue of int64
let registerName = RegisterName
let registerValue r = FromRegister <| RegisterName r
let integerValue i = RawValue i

type Instruction = 
    | Snd of Value
    | Set of (RegisterName * Value)
    | Add of (RegisterName * Value)
    | Mul of (RegisterName * Value)
    | Mod of (RegisterName * Value)
    | Rcv of RegisterName
    | Jgz of (Value * Value)

type State = { LastSoundPlayed : int64 option; Recovered : int64 option; Pointer : int64; Registers : Registers }
let init = { LastSoundPlayed = None; Recovered = None; Pointer = 0L; Registers = Map.empty }

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
    | 's'::'n'::'d' :: ' ' :: freq -> Snd <| parseValue freq
    | 's'::'e'::'t' :: ' ' :: reg :: ' ' :: v -> Set <| (registerName reg, parseValue v)
    | 'a'::'d'::'d' :: ' ' :: reg :: ' ' :: v -> Add <| (registerName reg, parseValue v)
    | 'm'::'u'::'l' :: ' ' :: reg :: ' ' :: v -> Mul <| (registerName reg, parseValue v)
    | 'm'::'o'::'d' :: ' ' :: reg :: ' ' :: v -> Mod <| (registerName reg, parseValue v)
    | 'r'::'c'::'v' :: ' ' :: reg :: [] -> Rcv <| registerName reg
    | 'j'::'g'::'z' :: ' ' :: v1 :: ' ' :: v2 -> Jgz <| (parseValue [v1] (*OUCH, incorrect when multi-digit*), parseValue v2)
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

let snd reg state = 
    let sound = 
        match reg with
        | FromRegister registerName -> getRegisterValue registerName state.Registers
        | RawValue rawValue -> rawValue
    { state with LastSoundPlayed = Some sound}
    |> incrementPointer 1L

let apply operator reg value state = 
    let oldValue = getRegisterValue reg state.Registers
    let v = getValue value state.Registers
    let newValue = operator oldValue v
    state 
    |> setValue reg newValue
    |> incrementPointer 1L

let add = apply (+)
let mul = apply (*)
let modulo = apply (%)

let rcv registerName state = 
    let v = getRegisterValue registerName state.Registers
    if v = 0L then
        state
        |> incrementPointer 1L
    else
        {state with Recovered = state.LastSoundPlayed}
        |> incrementPointer 1L

let jgz cond offset state =
    let v = getValue cond state.Registers
    if v > 0L then
        let offsetV = getValue offset state.Registers
        state
        |> incrementPointer offsetV
    else
        state
        |> incrementPointer 1L

let runInstruction instruction state = 
    match instruction with
    | Snd reg -> snd reg state
    | Set (reg, value) -> set reg value state
    | Add (reg, value) -> add reg value state
    | Mul (reg, value) -> mul reg value state
    | Mod (reg, value) -> modulo reg value state
    | Rcv reg -> rcv reg state
    | Jgz (cond, offset) -> jgz cond offset state

let print state =
    let registers =
        state.Registers
        |> Map.toList
        |> List.map (fun (RegisterName r,v) -> sprintf "(%A,%d)" r v)
        |> String.concat ";"
    sprintf 
        "Pointer: %d, LastPlayed: %A, Recovered: %A, Registers: %s" 
        state.Pointer 
        state.LastSoundPlayed 
        state.Recovered 
        registers

let rec run instructions state =
    let l = instructions |> Seq.length |> int64
    if state.Pointer < 0L || state.Pointer >= l then
        state
    else
        let i = instructions |> List.item (int state.Pointer)
        //printfn "Executing %A on %s" i (print state)
        let newState = runInstruction i state
        match i with
        | Rcv _ when Option.isSome newState.Recovered -> newState
        | _ -> run instructions newState

printf "Testing..."

test <@ parse "snd 1" = [Snd <| integerValue 1L] @>
test <@ parse "snd z" = [Snd <| registerValue 'z'] @>
test <@ parse "set a 3" = [Set <| (registerName 'a', integerValue 3L)] @>
test <@ parse "set a c" = [Set <| (registerName 'a', registerValue 'c')] @>
test <@ parse "add g 331" = [Add <| (registerName 'g', integerValue 331L)] @>
test <@ parse "add g x" = [Add <| (registerName 'g', registerValue 'x')] @>
test <@ parse "mul q -28" = [Mul <| (registerName 'q', integerValue -28L)] @>
test <@ parse "mul r t" = [Mul <| (registerName 'r', registerValue 't')] @>
test <@ parse "mod z 89" = [Mod <| (registerName 'z', integerValue 89L)] @>
test <@ parse "mod v p" = [Mod <| (registerName 'v', registerValue 'p')] @>
test <@ parse "rcv z" = [Rcv <| registerName 'z'] @>
test <@ parse "jgz 1 2" = [Jgz <| (integerValue 1L, integerValue 2L)] @>
test <@ parse "jgz i b" = [Jgz <| (registerValue 'i', registerValue 'b')] @>
test <@ parse "jgz k 0" = [Jgz <| (registerValue 'k', integerValue 0L)] @>
test <@ parse "snd a\nsnd b" = [Snd <| registerValue 'a'; Snd <| registerValue 'b'] @>

test <@ (run [Set (registerName 'a', integerValue 10L)] init).Registers |> Map.find (RegisterName 'a') = 10L @>
test <@ (run [Set (registerName 'a', integerValue 10L); Set (registerName 'b', registerValue 'a')] init).Registers |> Map.find (RegisterName 'b') = 10L @>

test <@ (run [Set (registerName 'a', integerValue 3L); Snd <| registerValue 'a'] init).LastSoundPlayed = Some 3L @>

test <@ (run [Set (registerName 'a', integerValue 3L); Add <| (registerName 'a', integerValue 2L)] init).Registers |> Map.find (RegisterName 'a') = 5L @>
test <@ (run [Set (registerName 'a', integerValue 3L); Set (registerName 'b', integerValue 2L); Add <| (registerName 'a', registerValue 'b')] init).Registers |> Map.find (RegisterName 'a') = 5L @>

test <@ (run [Set (registerName 'a', integerValue 3L); Mul <| (registerName 'a', integerValue 2L)] init).Registers |> Map.find (RegisterName 'a') = 6L @>
test <@ (run [Set (registerName 'a', integerValue 3L); Set (registerName 'b', integerValue 2L); Mul <| (registerName 'a', registerValue 'b')] init).Registers |> Map.find (RegisterName 'a') = 6L @>

test <@ (run [Set (registerName 'a', integerValue 5L); Mod <| (registerName 'a', integerValue 2L)] init).Registers |> Map.find (RegisterName 'a') = 1L @>
test <@ (run [Set (registerName 'a', integerValue 5L); Set (registerName 'b', integerValue 2L); Mod <| (registerName 'a', registerValue 'b')] init).Registers |> Map.find (RegisterName 'a') = 1L @>

test <@ (run [Rcv <| registerName 'a'] init).Recovered = None @>
test <@ (run [Set (registerName 'a', integerValue 1L); Rcv <| registerName 'a'] init).Recovered = None @>
test <@ (run [Set (registerName 'a', integerValue 1L); Snd <| integerValue 1337L; Rcv <| registerName 'a'] init).Recovered = Some 1337L @>

test <@ (run [Jgz <| (integerValue 1L, integerValue 3L)] init).Pointer = 3L @>
test <@ (run [Set (registerName 'a', integerValue 3L); Jgz <| (integerValue 1L, registerValue 'a')] init).Pointer = 4L @>
test <@ (run [Set (registerName 'a', integerValue 1L); Jgz <| (registerValue 'a', integerValue 3L)] init).Pointer = 4L @>
test <@ (run [Set (registerName 'a', integerValue 1L);  Set (registerName 'b', integerValue 3L); Jgz <| (registerValue 'a', registerValue 'b')] init).Pointer = 5L @>
printfn "Done"

let example = "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"

test <@ Some 4L = (run (example |> parse) init).Recovered @>

let input = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt" )
let instructions = input |> parse
let part1 = run instructions init