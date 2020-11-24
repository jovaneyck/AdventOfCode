#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Program = int list
type state = 
    | Running of Program
    | Finished of Program
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

let parseInstructionAt address program =
    let atPointer = program |> List.skip address
    //printfn "program %A pointer %A atPointer %A" program address atPointer
    let reversed = atPointer |> List.head |> string |> Seq.toList |> List.rev
    let opcode = reversed
    let modes opLength =
        let parsedModes = reversed |> List.skip opLength |> List.map (System.Char.GetNumericValue >> int) 
            
        List.append 
            parsedModes
            (List.replicate 3 0)
        |> List.map (function | 0 -> Position | 1 -> Immediate)

    match opcode with
    | '9' :: '9' :: _ -> Halt
    | '1' :: '0' :: _ -> 
        let [a;b;(_, r)] = atPointer |> List.skip 1 |> List.take 3 |> List.zip (modes 2 |> List.take 3)
        Add (a,b,r)
    | '1' :: _ -> 
        let [a;b;(_, r)] = atPointer |> List.skip 1 |> List.take 3 |> List.zip (modes 1 |> List.take 3)
        Add (a,b,r)
    | '2' :: '0' :: _ -> 
        let [a;b;(_, r)] = atPointer |> List.skip 1 |> List.take 3 |> List.zip (modes 2 |> List.take 3)
        Multiply (a,b,r)
    | '2' :: _ -> 
        let [a;b;(_, r)] = atPointer |> List.skip 1 |> List.take 3 |> List.zip (modes 1 |> List.take 3)
        Multiply (a,b,r)
    | '4' :: '0' :: _ -> 
        let [r] = atPointer |> List.skip 1 |> List.take 1 |> List.zip (modes 2 |> List.take 1)
        Output r
    | '4' :: _ -> 
        let [r] = atPointer |> List.skip 1 |> List.take 1 |> List.zip (modes 1 |> List.take 1)
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

let runSingleInstruction program =
    let v = valueAt program
    function
    | Halt -> program |> Finished
    | Add(a,b,r) ->
        let sum = (v a) + (v b)
        store sum r program |> Running
    | Multiply(a,b,r) ->
        let product = (v a) * (v b)
        store product r program |> Running
    //Goodbye purity, no time for that nonsense
    | Input r ->
        printfn "INPUT:"
        let i = System.Console.ReadLine() |> int
        store i r program |> Running
    | Output r ->
        printfn "OUTPUT: %A" (v r)
        program |> Running

let pointerIncrement i =
    match i with
    | Halt -> 1
    | Add _
    | Multiply _ -> 4
    | Input _
    | Output _ -> 2

let rec run pointer program =
    match program with
    | Finished p -> p
    | Running p -> 
        let i = parseInstructionAt pointer p
        runSingleInstruction p i
        |> (run (pointer + (pointerIncrement i)))

let programWithInput (noun,verb) p =
    let a :: b :: c :: ps = p
    Running (a :: noun :: verb :: ps)

printf "Testing..."

//Parameter modes
test <@ parseInstructionAt 0 [10001;9;10;3;2;3;11;0;99;30;40;50] = Add((Position, 9),(Position, 10),3) @>
test <@ parseInstructionAt 4 [10001;9;10;3;10002;3;11;0;99;30;40;50] = Multiply((Position, 3),(Position, 11),0) @>
test <@ runSingleInstruction [1;9;10;3;2;3;11;0;99;30;40;50] (Add((Position, 9),(Position, 10),3)) = Running [1; 9; 10; 70; 2; 3; 11; 0; 99; 30; 40; 50] @>

test <@ parseInstructionAt 0 [1002;4;3;4;33] = Multiply ((Position, 4),(Immediate, 3),4) @>

//New instructions
test <@ parseInstructionAt 0 [3;225] = Input 225 @>
test <@ parseInstructionAt 0 [4;225] = Output (Position, 225) @>
test <@ parseInstructionAt 0 [104;225] = Output (Immediate, 225) @>

test <@ Running [10001;9;10;3;10002;3;11;0;99;30;40;50] |> run 0 = [3500; 9; 10; 70; 10002; 3; 11; 0; 99; 30; 40; 50]  @>

//Variable instruction length/IP increment
test <@ pointerIncrement (Add((Position, 9),(Position, 10),3)) = 4 @>
test <@ pointerIncrement (Input 1) = 2 @>
test <@ pointerIncrement (Output (Position, 1)) = 2 @>
test <@ pointerIncrement Halt = 1 @>

printfn "..done!"

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : Program = input.Split([|','|]) |> Seq.map int |> Seq.toList

run 0 (Running p)