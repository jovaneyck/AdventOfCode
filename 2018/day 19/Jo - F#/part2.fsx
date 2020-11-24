#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Register = Register of int
type Device = { registers : Map<Register,int>; instructionpointer : int; boundRegister : int option}
type OpCode = 
    | Addr
    | Addi
    | Mulr
    | Muli
    | Banr
    | Bani
    | Borr
    | Bori
    | Setr
    | Seti
    | Gtir
    | Gtri
    | Gtrr
    | Eqir
    | Eqri
    | Eqrr

type Instruction = { opcode : OpCode; a : int; b : int; c : Register }

let deviceWith registerValues = 
    { registers = (List.zip  ([0..5] |> List.map (Register)) registerValues ) |> Map.ofList; instructionpointer = 0; boundRegister = None }
let instruction oc a b c = {a = a; b = b; c = Register c; opcode = oc}

let registerValue device index = device.registers |> Map.find index
let update register newValue registers = registers |> Map.add register newValue

let addRegister device instruction = 
    let a = registerValue device (Register instruction.a)
    let b = registerValue device (Register instruction.b)
    { device with registers = device.registers |> update instruction.c (a+b) }

let addImmediate device instruction =
    let a = registerValue device (Register instruction.a)
    { device with registers = device.registers |> update instruction.c (a+instruction.b) }

let multiplyRegister device instruction = 
    let a = registerValue device (Register instruction.a)
    let b = registerValue device (Register instruction.b)
    { device with registers = device.registers |> update instruction.c (a*b) }

let multiplyImmediate device instruction =
    let a = registerValue device (Register instruction.a)
    { device with registers = device.registers |> update instruction.c (a*instruction.b) }

let bitwiseAndRegister device instruction = 
    let a = registerValue device (Register instruction.a)
    let b = registerValue device (Register instruction.b)
    { device with registers = device.registers |> update instruction.c (a &&& b) }

let bitwiseAndImmediate device instruction =
    let a = registerValue device (Register instruction.a)
    { device with registers = device.registers |> update instruction.c (a &&& instruction.b) }

let bitwiseOrRegister device instruction = 
    let a = registerValue device (Register instruction.a)
    let b = registerValue device (Register instruction.b)
    { device with registers = device.registers |> update instruction.c (a ||| b) }

let bitwiseOrImmediate device instruction =
    let a = registerValue device (Register instruction.a)
    { device with registers = device.registers |> update instruction.c (a ||| instruction.b) }

let setRegister device instruction = 
    let a = registerValue device (Register instruction.a)
    { device with registers = device.registers |> update instruction.c a }

let setImmediate device instruction = 
    { device with registers = device.registers |> update instruction.c instruction.a }

let greaterThanImmediateRegister device instruction =
    let a = instruction.a
    let b = registerValue device (Register instruction.b)
    let result = if a > b then 1 else 0
    { device with registers = device.registers |> update instruction.c result }

let greaterThanRegisterImmediate device instruction =
    let a = registerValue device (Register instruction.a)
    let b = instruction.b
    let result = if a > b then 1 else 0
    { device with registers = device.registers |> update instruction.c result }

let greaterThanRegisterRegister device instruction =
    let a = registerValue device (Register instruction.a)
    let b = registerValue device (Register instruction.b)
    let result = if a > b then 1 else 0
    { device with registers = device.registers |> update instruction.c result }

let equalImmediateRegister device instruction =
    let a = instruction.a
    let b = registerValue device (Register instruction.b)
    let result = if a = b then 1 else 0
    { device with registers = device.registers |> update instruction.c result }

let equalRegisterImmediate device instruction =
    let a = registerValue device (Register instruction.a)
    let b = instruction.b
    let result = if a = b then 1 else 0
    { device with registers = device.registers |> update instruction.c result }

let equalRegisterRegister device instruction =
    let a = registerValue device (Register instruction.a)
    let b = registerValue device (Register instruction.b)
    let result = if a = b then 1 else 0
    { device with registers = device.registers |> update instruction.c result }

let handlers =
    [
        (Addr, addRegister)
        (Addi, addImmediate)

        (Mulr, multiplyRegister)
        (Muli, multiplyImmediate)

        (Banr, bitwiseAndRegister)
        (Bani, bitwiseAndImmediate)

        (Borr, bitwiseOrRegister)
        (Bori, bitwiseOrImmediate)

        (Setr, setRegister)
        (Seti, setImmediate)

        (Gtir, greaterThanImmediateRegister)
        (Gtri, greaterThanRegisterImmediate)
        (Gtrr, greaterThanRegisterRegister)

        (Eqir, equalImmediateRegister)
        (Eqri, equalRegisterImmediate)
        (Eqrr, equalRegisterRegister)
    ]
    |> Map.ofSeq

let runInstruction device instruction =
    (handlers |> Map.find instruction.opcode) device instruction

let parseOpcode = 
    function
    | "addr" -> Addr
    | "addi" -> Addi
    | "mulr" -> Mulr
    | "muli" -> Muli
    | "banr" -> Banr
    | "bani" -> Bani
    | "borr" -> Borr
    | "bori" -> Bori
    | "setr" -> Setr
    | "seti" -> Seti
    | "gtir" -> Gtir
    | "gtri" -> Gtri
    | "gtrr" -> Gtrr
    | "eqir" -> Eqir
    | "eqri" -> Eqri
    | "eqrr" -> Eqrr
    | unknown -> failwithf "Unknown opcode: %s" unknown

let parseInstruction (rawInstruction : string) =
    let numbers = rawInstruction.Split(' ') |> Seq.toList
    instruction (parseOpcode numbers.[0]) (int numbers.[1]) (int numbers.[2]) (int numbers.[3])

let parse initialRegisters (input :string list) = 
    let boundRegister = (input.[0]).Substring(4) |> int
    let instructions = input.[1..] |> List.map parseInstruction
    ({ deviceWith initialRegisters with boundRegister = Some boundRegister }, instructions)

let rec loop generation instructions device = 
    //if generation % 1000 = 0 then
    printfn "%d: %A" generation (device.registers |> Map.toList |> List.map snd)
    match instructions |> List.tryItem device.instructionpointer with
    | None -> device
    | Some i ->
        let patched = 
            match device.boundRegister with
            | Some r -> { device with registers = device.registers |> update (Register r) device.instructionpointer }
            | None -> device
        let ran = runInstruction patched i
        let next = 
            match ran.boundRegister with
            | None -> { ran with instructionpointer = ran.instructionpointer + 1 }
            | Some r -> { ran with instructionpointer = (ran.registers |> Map.find (Register r)) + 1 }
        loop (generation + 1) instructions next

let run initialRegisters input = 
    let device, instructions = input |> parse initialRegisters
    loop 0 instructions device

let example = [
                "#ip 0"
                "seti 5 0 1"
                "seti 6 0 2"
                "addi 0 1 0"
                "addr 1 2 3"
                "setr 1 0 0"
                "seti 8 0 4"
                "seti 9 0 5"]

let tt () =
    printf "Testing..."
    test <@ (run [0;0;0;0;0;0] example).registers |> Map.toList |> List.map snd = [6; 5; 6; 0; 0; 9]  @> //registers at end
    printfn "..done!"  

tt ()

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt").Split([|"\n";"\r"|], System.StringSplitOptions.RemoveEmptyEntries) |> Seq.toList
let result = run [1;0;0;0;0;0] input
result.registers |> Map.toList |> List.map snd |> List.head