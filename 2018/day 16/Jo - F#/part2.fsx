#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
open System.Text.RegularExpressions

type Register = Register of int
type Device = { registers : Map<Register,int>}
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

type Instruction = { opcode :int; a : int; b : int; c : Register }
type Sample = { before : Device; after : Device; instruction : Instruction }

let deviceWith registerValues = 
    { registers = (List.zip  ([0..3] |> List.map (Register)) registerValues ) |> Map.ofList }
let instruction a b c = {a = a; b = b; c = Register c; opcode = -1}

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

let runInstruction opcode device instruction =
    (handlers |> Map.find opcode) device instruction

let parseInstruction (instruction : string) =
    let numbers = instruction.Split(' ') |> Seq.toList |> List.map int
    { opcode = int numbers.[0]; a = int numbers.[1]; b = int numbers.[2]; c = Register (int numbers.[3])}
let parseSample sample =
    let get (idx: int) (m : Match) = m.Groups.[idx].Value |> int          
    let parseBefore before =
        let m = Regex.Match(before, "Before: \[(\d*), (\d*), (\d*), (\d*)\]")
        deviceWith [get 1 m; get 2 m; get 3 m; get 4 m]
    let parseAfter after =
        let m = Regex.Match(after, "After:  \[(\d*), (\d*), (\d*), (\d*)\]")
        deviceWith [get 1 m; get 2 m; get 3 m; get 4 m]
    

    let (rawBefore :: rawInstruction :: rawAfter :: []) = sample
    { before = parseBefore rawBefore
      after = parseAfter rawAfter 
      instruction = parseInstruction rawInstruction}

let runAllOpcodes device instruction = 
    handlers
    |> Map.toList
    |> List.map (fun (oc, handler) -> (oc, handler device instruction))

let behavesLike sample =
    let results = runAllOpcodes sample.before sample.instruction
    let matches =
        results
        |> List.filter (fun (_, result) -> result = sample.after)
    (sample.instruction.opcode, matches |> List.map fst)
    

let example= ["Before: [3, 2, 1, 1]";"9 2 1 2";"After:  [3, 2, 2, 1]"]
let tt () =
    printf "Testing..."
    test <@ runInstruction Addr (deviceWith [1..4]) (instruction 1 2 3) = deviceWith [1;2;3;5] @>
    test <@ runInstruction Addi (deviceWith [1..4]) (instruction 1 8 3) = deviceWith [1;2;3;10] @>
    test <@ runInstruction Mulr (deviceWith [1..4]) (instruction 1 2 3) = deviceWith [1;2;3;6] @>
    test <@ runInstruction Muli (deviceWith [1..4]) (instruction 1 10 3) = deviceWith [1;2;3;20] @>
    test <@ runInstruction Banr (deviceWith [1..4]) (instruction 0 2 3) = deviceWith [1;2;3;1] @>
    test <@ runInstruction Bani (deviceWith [1..4]) (instruction 0 2 3) = deviceWith [1;2;3;0] @>
    test <@ runInstruction Borr (deviceWith [1..4]) (instruction 0 1 3) = deviceWith [1;2;3;3] @>
    test <@ runInstruction Bori (deviceWith [1..4]) (instruction 0 10 3) = deviceWith [1;2;3;11] @>
    test <@ runInstruction Setr (deviceWith [1337;2;3;4]) (instruction 0 -1 3) = deviceWith [1337;2;3;1337] @>
    test <@ runInstruction Seti (deviceWith [1;2;3;4]) (instruction 666 -1 3) = deviceWith [1;2;3;666] @>
    test <@ runInstruction Gtir (deviceWith [1;2;3;4]) (instruction 2 0 3) = deviceWith [1;2;3;1] @>
    test <@ runInstruction Gtir (deviceWith [1;2;3;4]) (instruction 0 0 3) = deviceWith [1;2;3;0] @>
    test <@ runInstruction Gtri (deviceWith [1;2;3;4]) (instruction 0 2 3) = deviceWith [1;2;3;0] @>
    test <@ runInstruction Gtri (deviceWith [1;2;3;4]) (instruction 0 0 3) = deviceWith [1;2;3;1] @>
    test <@ runInstruction Gtrr (deviceWith [1;2;3;4]) (instruction 0 1 3) = deviceWith [1;2;3;0] @>
    test <@ runInstruction Gtrr (deviceWith [1;2;3;4]) (instruction 1 0 3) = deviceWith [1;2;3;1] @>
    test <@ runInstruction Eqir (deviceWith [1;2;3;4]) (instruction 0 0 3) = deviceWith [1;2;3;0] @>
    test <@ runInstruction Eqir (deviceWith [1;2;3;4]) (instruction 3 2 3) = deviceWith [1;2;3;1] @>
    test <@ runInstruction Eqri (deviceWith [1;2;3;4]) (instruction 0 0 3) = deviceWith [1;2;3;0] @>
    test <@ runInstruction Eqri (deviceWith [1;2;3;4]) (instruction 0 1 3) = deviceWith [1;2;3;1] @>
    test <@ runInstruction Eqrr (deviceWith [1;2;3;4]) (instruction 0 1 3) = deviceWith [1;2;3;0] @>
    test <@ runInstruction Eqrr (deviceWith [3;2;3;4]) (instruction 0 2 3) = deviceWith [3;2;3;1] @>
    test <@ parseSample example = { before = deviceWith [3;2;1;1]; after = deviceWith [3;2;2;1]; instruction = {a= 2; b= 1; c= Register 2; opcode=9 } } @>
    test <@ parseSample example |> behavesLike = (9, [Addi; Mulr; Seti]) @>
    printfn "..done!"  

tt ()

let input = 
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
        .Split([|"\n";"\r"|], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Seq.toList

let samples =
    input 
    |> List.chunkBySize 3
    |> List.map parseSample

//looks like you can narrow them down step by step, there's always one inambiguous opcode if you strip all knowns
//I looped through filling up the map of knowns + cross-checking them with the samples a couple of times by hand
let opcodeMap = 
    Map.ofList 
        [
            (0, Addr)
            (1, Eqri)
            (2, Eqir)
            (3, Eqrr)
            (4, Gtir)
            (5, Addi)
            (6, Banr)
            (7, Gtri)
            (8, Bori)
            (9, Muli)
            (10, Seti)
            (11, Gtrr)
            (12, Setr)
            (13, Borr)
            (14, Mulr)
            (15, Bani)
        ]

let behaviorsPerOpcode =
    samples
    |> List.map behavesLike
    |> List.map (fun (oc,matches) -> oc, matches |> List.except (opcodeMap |> Map.toList |> List.map snd))
    |> List.filter (fun (_, m) -> m |> Seq.isEmpty |> not)
    |> List.distinct
    |> List.sortBy (snd >> List.length)
    
let program = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\program.txt") |> Seq.toList
let instructions = program |> List.map parseInstruction
let run = instructions |> List.fold (fun d i -> runInstruction (opcodeMap |> Map.find i.opcode) d i) (deviceWith [0;0;0;0])