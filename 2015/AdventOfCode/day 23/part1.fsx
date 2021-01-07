#r "nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"

type Register = A | B
type JumpData = { register : Register; offset : int }
type Instruction = 
    | Half of Register 
    | Triple of Register
    | Increment of Register
    | Jump of int
    | JumpIfEven of JumpData
    | JumpIfOne of JumpData

type Registers = { a : uint; b : uint }
type Machine = { instructionpointer : int; registers : Registers; program : Instruction array }

let init program = { instructionpointer = 0; registers = {a = 0u; b = 0u}; program = program }

let parseRegister = function "a" -> A | "b" -> B | unknown -> failwithf "Unknown register: %s" unknown

let (|Hlf|_|) line = 
    let re = Regex("hlf (\w)")
    let m = re.Match line
    match m.Success with
    | false -> None
    | true -> m.Groups.[1].Value |> parseRegister |> Some
    
let (|Tpl|_|) line = 
    let re = Regex("tpl (\w)")
    let m = re.Match line
    match m.Success with
    | false -> None
    | true -> m.Groups.[1].Value |> parseRegister |> Some

let (|Inc|_|) line = 
    let re = Regex("inc (\w)")
    let m = re.Match line 
    match m.Success with
    | false -> None
    | true -> m.Groups.[1].Value |> parseRegister |> Some
    
let (|Jmp|_|) line = 
    let re = Regex("jmp (-?\+?\d*)")
    let m = re.Match line
    match m.Success with
    | false -> None
    | true -> m.Groups.[1].Value |> int |> Some
    
let (|Jie|_|) line = 
    let re = Regex("jie (\w), (-?\+?\d*)")
    let m = re.Match line
    match m.Success with
    | false -> None
    | true -> 
        let r = m.Groups.[1].Value |> parseRegister 
        let offset = m.Groups.[2].Value |> int 
        Some { register = r; offset = offset }

let (|Jio|_|) line = 
    let re = Regex("jio (\w), (-?\+?\d*)")
    let m = re.Match line
    match m.Success with
    | false -> None
    | true -> 
        let r = m.Groups.[1].Value |> parseRegister 
        let offset = m.Groups.[2].Value |> int 
        Some { register = r; offset = offset }

let parseInstruction =
    function
    | Hlf r -> Half r
    | Tpl r -> Triple r
    | Inc r -> Increment r
    | Jmp offset -> Jump offset
    | Jie data -> JumpIfEven data
    | Jio data -> JumpIfOne data
    | unknown -> failwithf "Failed to parse instruction: %s" unknown

let parseInstructions text =
    text |> Seq.map parseInstruction |> Seq.toList

let read register machine =
    match register with
    | A -> machine.registers.a
    | B -> machine.registers.b
    
let write machine register value = 
    match register with
    | A -> { machine with registers = {machine.registers with a = value } }
    | B -> { machine with registers = {machine.registers with b = value } }

let execute machine =
    let ip offset machine = { machine with instructionpointer = machine.instructionpointer + offset }

    function
    | Half r -> machine |> read r |> (fun v -> v / 2u) |> write machine r |> ip 1
    | Triple r -> machine |> read r |> (fun v -> 3u * v) |> write machine r |> ip 1
    | Increment r -> machine |> read r |> (fun v -> 1u + v) |> write machine r |> ip 1
    | Jump o -> machine |> ip o
    | JumpIfEven {register = r; offset = offset } -> 
        let value = machine |> read r
        if value % 2u = 0u 
        then machine |> ip offset
        else machine |> ip 1
    | JumpIfOne {register = r; offset = offset } -> 
        let value = machine |> read r
        if value = 1u 
        then machine |> ip offset
        else machine |> ip 1

let rec run machine =
    match machine.program |> Array.tryItem machine.instructionpointer with
    | None -> machine
    | Some instruction ->
        //printfn "ip: %d, a: %d, b: %d, instruction: %A" machine.instructionpointer (machine |> read A) (machine |> read B) instruction
        let next = execute machine instruction
        run next

let solve input =
    let instructions = input |> parseInstructions
    let machine = init (Seq.toArray instructions)
    let result = run machine
    result

let part1 = solve input
part1 |> read B |> printfn "%d"
let t () = 
    printf "Testing.."
    test <@ parseInstructions ["hlf a"; "hlf b"] = [Half A; Half B] @>
    test <@ parseInstructions ["tpl a"; "tpl b"] = [Triple A; Triple B] @>
    test <@ parseInstructions ["inc a"] = [Increment A] @>
    test <@ parseInstructions ["jmp -7"] = [Jump -7] @>
    test <@ parseInstructions ["jmp +3"] = [Jump 3] @>
    test <@ parseInstructions ["jmp +0"] = [Jump 0] @>
    test <@ parseInstructions ["jie a, -3"] = [JumpIfEven {register = A; offset = -3} ] @>
    test <@ parseInstructions ["jie a, +3"] = [JumpIfEven {register = A; offset = 3} ] @>
    test <@ parseInstructions ["jio b, -3"] = [JumpIfOne {register = B; offset = -3} ] @>
    test <@ parseInstructions ["jio b, +3"] = [JumpIfOne {register = B; offset = 3} ] @>
    test <@ solve example |> read A = 2u @>
    test <@ write (init [||]) A 33u |> read B = 0u @>
    test <@ write (init [||]) A 33u |> read A = 33u @>

    test <@ execute (write (init [||]) B 30u) (Half B) |> read B = 15u @>
    test <@ execute (write (init [||]) B 30u) (Triple B) |> read B = 90u @>
    test <@ execute (write (init [||]) B 30u) (Increment B) |> read B = 31u @>
    printfn "..done!"

t ()