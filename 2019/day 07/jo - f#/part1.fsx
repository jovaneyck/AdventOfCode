#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Pointer = int
type Program = int list
type ProgramMode = 
    | Running
    | Finished
type State = { program : Program; mode : ProgramMode; pointer : Pointer; input : int list; output: int list }
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
        let [a;b] = parseArgs 2 2
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
    | _ :: xs when address = 0 -> value :: xs
    | x :: xs -> x :: (store value (address - 1) xs)
    | err -> failwithf "Store out of bounds: %A" err

let pointerIncrement =
    function
    | Halt -> 1
    | Input _ -> 2
    | Output _ -> 2
    | JumpIfTrue _ -> 3
    | JumpIfFalse _ -> 3
    | Add _ -> 4
    | Multiply _ -> 4
    | LessThan _ -> 4
    | Equal _ -> 4

let runSingleInstruction state instruction =
    let v = valueAt state.program
    match instruction with
    | Halt -> 
        { state with 
            mode = Finished
            pointer = state.pointer + pointerIncrement instruction }
    | Add(a,b,r) ->
        let sum = (v a) + (v b)
        { state with 
            program = store sum r state.program
            pointer = state.pointer + pointerIncrement instruction }
    | Multiply(a,b,r) ->
        let product = (v a) * (v b)
        { state with 
            program = store product r state.program
            pointer = state.pointer + pointerIncrement instruction }
    | Input r ->
        let i::is = state.input
        { state with 
            program = store i r state.program
            pointer = state.pointer + pointerIncrement instruction
            input = is }
    | Output r ->
        let o = v r
        { state with 
            pointer = state.pointer + pointerIncrement instruction
            output = o :: state.output }
    | Equal (a,b,r) ->
        let one = v a
        let other = v b
        let result = if one = other then 1 else 0
        { state with 
            program = store result r state.program
            pointer = state.pointer + pointerIncrement instruction }
    | JumpIfTrue (a,r) ->
        { state with 
            pointer = if v a <> 0 then v r else  state.pointer + pointerIncrement instruction }
    | JumpIfFalse (a,r) ->
        { state with 
            pointer = if v a = 0 then v r else  state.pointer + pointerIncrement instruction }
    | LessThan (a,b,r) ->
        let result = if (v a) < (v b) then 1 else 0
        { state with 
            program = store result r state.program
            pointer = state.pointer + pointerIncrement instruction }

let rec run (state : State) : State =
    match state.mode with
    | Finished -> state
    | Running -> 
        let i = parseInstructionAt state.pointer state.program
        runSingleInstruction state i
        |> run

let initState inputs program =
    { program = program; mode = Running; pointer = 0; input = inputs; output = [] }

let rec permute l = 
  let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

  match l with
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let rec trial program (input : int) phases =
    match phases with
    | [] -> input
    | p::ps ->
        let [output] = initState [p;input] program |> run |> fun s -> s.output
        trial program output ps

let solve program =
    let candidates = [0..4] |> permute
    candidates
    |> List.map (trial program 0)
    |> List.max

printf "Testing..."

//Parameter modes
test <@ parseInstructionAt 0 [10001;9;10;3;2;3;11;0;99;30;40;50] = Add((Position, 9),(Position, 10),3) @>
test <@ parseInstructionAt 4 [10001;9;10;3;10002;3;11;0;99;30;40;50] = Multiply((Position, 3),(Position, 11),0) @>
test <@ parseInstructionAt 0 [1002;4;3;4;33] = Multiply ((Position, 4),(Immediate, 3),4) @>

//New instructions
test <@ parseInstructionAt 0 [3;225] = Input 225 @>
test <@ parseInstructionAt 0 [4;225] = Output (Position, 225) @>
test <@ parseInstructionAt 0 [104;225] = Output (Immediate, 225) @>
test <@ parseInstructionAt 0 [8;9;10;9] = Equal ((Position, 9),(Position, 10),9) @>
test <@ parseInstructionAt 0 [1108;7;8;3] = Equal ((Immediate, 7),(Immediate, 8),3) @>

test <@ 
        let s = runSingleInstruction (initState [] [1;9;10;3;2;3;11;0;99;30;40;50])  (Add((Position, 9),(Position, 10),3)) 
        s.program = [1; 9; 10; 70; 2; 3; 11; 0; 99; 30; 40; 50] && s.pointer = 4 @>
test <@ initState [] [10001;9;10;3;10002;3;11;0;99;30;40;50] |> run |> (fun s -> (s.mode, s.program)) = (Finished, [3500; 9; 10; 70; 10002; 3; 11; 0; 99; 30; 40; 50])  @>

//Variable instruction length/IP increment
test <@ pointerIncrement (Add((Position, 9),(Position, 10),3)) = 4 @>
test <@ pointerIncrement (Input 1) = 2 @>
test <@ pointerIncrement (Output (Position, 1)) = 2 @>
test <@ pointerIncrement Halt = 1 @>

//IO
test <@ initState [1337;666] [3;1;4;1;99] |> run |> (fun s -> s.input, s.output) = ([666], [1337]) @>

test <@ permute [1;2;3] = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]] @>

test <@ solve [3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0] = 43210 @>
test <@ solve [3;23;3;24;1002;24;10;24;1002;23;-1;23;101;5;23;23;1;24;23;23;4;23;99;0;0] = 54321 @>
test <@ solve [3;31;3;32;1002;32;10;32;1001;31;-2;31;1007;31;0;33;1002;33;7;33;1;33;31;31;1;32;31;31;4;31;99;0;0;0] = 65210 @>

printfn "..done!"

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : Program = input.Split([|','|]) |> Seq.map int |> Seq.toList

solve p