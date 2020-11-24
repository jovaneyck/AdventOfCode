#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Pointer = int
type Program = int list
type ProgramMode = 
    | Running of Program
    | Finished of Program
type State =
    ProgramMode * Pointer
type ParameterMode =
    | Position
    | Immediate
type Parameter = ParameterMode * int
type Instruction = 
    | Add of Parameter * Parameter * int
    | Multiply of Parameter * Parameter * int
    | Halt
    | Input of int
    | Output of Parameter
    | Equal of Parameter * Parameter * int
    | LessThan of Parameter * Parameter * int
    | JumpIfTrue of Parameter * Parameter
    | JumpIfFalse of Parameter * Parameter

let zipInfinite otherInfinity one =
    otherInfinity 
    |> Seq.take (one |> Seq.length) 
    |> Seq.toList
    |> List.zip one 
    |> List.map (fun (f,s) -> (s,f))

let parseInstructionAt address program =
    let atPointer = program |> List.skip address
    let reversedInstruction = atPointer |> List.head |> string |> Seq.toList |> List.rev
    let opcode = reversedInstruction
    let modes opLength =
        let parsedModes = reversedInstruction |> List.skip opLength |> List.map (System.Char.GetNumericValue >> int) 
            
        List.append parsedModes (List.replicate 3 0)
        |> List.map (function | 0 -> Position | 1 -> Immediate)

    let parseArgs opSize nbArgs = atPointer |> List.skip 1 |> List.take nbArgs |> zipInfinite (modes opSize) 

    match opcode with
    | '9' :: '9' :: _ -> Halt
    | '8' :: '0' :: _ -> 
        let [a;b;(_,r)] = parseArgs 2 3
        Equal (a,b,r)
    | '8' :: _ -> 
        let [a;b;(_,r)] = parseArgs 1 3
        Equal (a,b,r)
    | '7' :: '0' :: _ -> 
        let [a;b;(_,r)] = parseArgs 2 3
        LessThan (a,b,r)
    | '7' :: _ -> 
        let [a;b;(_,r)] = parseArgs 1 3
        LessThan (a,b,r)
    | '6' :: '0' :: _ -> 
        let [a;b] = parseArgs 2 2
        JumpIfFalse (a,b)
    | '6' :: _ -> 
        let [a;b] = parseArgs 1 2
        JumpIfFalse (a,b)
    | '5' :: '0' :: _ -> 
        let [a;b] = parseArgs 2 3
        JumpIfTrue (a,b)
    | '5' :: _ -> 
        let [a;b] = parseArgs 1 2
        JumpIfTrue (a,b)
    | '1' :: '0' :: _ -> 
        let [a;b;(_, r)] = parseArgs 2 3
        Add (a,b,r)
    | '1' :: _ -> 
        let [a;b;(_, r)] = parseArgs 1 3
        Add (a,b,r)
    | '2' :: '0' :: _ -> 
        let [a;b;(_, r)] = parseArgs 2 3
        Multiply (a,b,r)
    | '2' :: _ -> 
        let [a;b;(_, r)] = parseArgs 1 3
        Multiply (a,b,r)
    | '4' :: '0' :: _ -> 
        let [r] = atPointer |> List.skip 1 |> List.take 1 |> zipInfinite (modes 2)
        Output r
    | '4' :: _ -> 
        let [r] = atPointer |> List.skip 1 |> List.take 1 |> zipInfinite (modes 1)
        Output r
    | '3' :: _ ->
        Input (atPointer |> List.skip 1 |> List.head)
    | err -> failwithf "Unknown opcode: %A" err

let valueAt program parameter = 
    match parameter with
    | Immediate, v -> v
    | Position, p -> program |> List.skip p |> List.head

let rec store value address program =
    match program with
    | x :: xs when address = 0 -> value :: xs
    | x :: xs -> x :: (store value (address - 1) xs)
    | err -> failwithf "Store out of bounds: %A" err

let pointerIncrement i =
    match i with
    | Halt -> 1
    | Add _ -> 4
    | Multiply _ -> 4
    | Input _ -> 2
    | Output _ -> 2
    | LessThan _ -> 4
    | Equal _ -> 4
    | JumpIfTrue _ -> 3
    | JumpIfFalse _ -> 3

let runSingleInstruction program pointer instruction =
    let v = valueAt program
    match instruction with
    | Halt -> (program |> Finished, pointer + pointerIncrement instruction)
    | Add(a,b,r) ->
        let sum = (v a) + (v b)
        (store sum r program |> Running, pointer + pointerIncrement instruction)
    | Multiply(a,b,r) ->
        let product = (v a) * (v b)
        (store product r program |> Running, pointer + pointerIncrement instruction)
    //Goodbye purity, no time for that nonsense
    | Input r ->
        printfn "INPUT:"
        let i = System.Console.ReadLine() |> int
        (store i r program |> Running, pointer + pointerIncrement instruction)
    | Output r ->
        printfn "OUTPUT: %A" (v r)
        (program |> Running, pointer + pointerIncrement instruction)
    | Equal (a,b,r) ->
        let one = v a
        let other = v b
        let result = if one = other then 1 else 0
        (store result r program |> Running, pointer + pointerIncrement instruction)
    | JumpIfTrue (a,r) ->
        if v a <> 0 then
            (Running program, v r)
        else (Running program, pointer + pointerIncrement instruction)
    | JumpIfFalse (a,r) ->
        if v a = 0 then
            (Running program, v r)
        else (Running program, pointer + pointerIncrement instruction)
    | LessThan (a,b,r) ->
        let result = if (v a) < (v b) then 1 else 0
        (store result r program |> Running, pointer + pointerIncrement instruction)

let rec run (state : State) : Program =
    let program, pointer = state
    match program with
    | Finished p -> p
    | Running p -> 
        let i = parseInstructionAt pointer p
        runSingleInstruction p pointer i
        |> run

printf "Testing..."

//Parameter modes
test <@ parseInstructionAt 0 [10001;9;10;3;2;3;11;0;99;30;40;50] = Add((Position, 9),(Position, 10),3) @>
test <@ parseInstructionAt 4 [10001;9;10;3;10002;3;11;0;99;30;40;50] = Multiply((Position, 3),(Position, 11),0) @>
test <@ runSingleInstruction [1;9;10;3;2;3;11;0;99;30;40;50] 0 (Add((Position, 9),(Position, 10),3)) = (Running [1; 9; 10; 70; 2; 3; 11; 0; 99; 30; 40; 50], 4) @>

test <@ parseInstructionAt 0 [1002;4;3;4;33] = Multiply ((Position, 4),(Immediate, 3),4) @>

//New instructions
test <@ parseInstructionAt 0 [3;225] = Input 225 @>
test <@ parseInstructionAt 0 [4;225] = Output (Position, 225) @>
test <@ parseInstructionAt 0 [104;225] = Output (Immediate, 225) @>
test <@ parseInstructionAt 0 [8;9;10;9] = Equal ((Position, 9),(Position, 10),9) @>
test <@ parseInstructionAt 0 [1108;7;8;3] = Equal ((Immediate, 7),(Immediate, 8),3) @>

test <@ run (Running [10001;9;10;3;10002;3;11;0;99;30;40;50],0) = [3500; 9; 10; 70; 10002; 3; 11; 0; 99; 30; 40; 50]  @>

//Variable instruction length/IP increment
test <@ pointerIncrement (Add((Position, 9),(Position, 10),3)) = 4 @>
test <@ pointerIncrement (Input 1) = 2 @>
test <@ pointerIncrement (Output (Position, 1)) = 2 @>
test <@ pointerIncrement Halt = 1 @>

printfn "..done!"

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : Program = input.Split([|','|]) |> Seq.map int |> Seq.toList

run ((Running p), 0) //OUTPUT: 7873292