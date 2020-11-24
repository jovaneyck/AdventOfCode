#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Program = int list
type state = 
    | Running of Program
    | Finished of Program
type Instruction = 
    | Add of int * int * int
    | Multiply of int * int * int
    | Halt

let parseInstructionAt address program =
    match program |> List.skip address with
    | 99 :: _ -> Halt
    | 1 :: a :: b :: r :: _ -> Add (a,b,r)
    | 2 :: a :: b :: r :: _ -> Multiply (a,b,r)
    | err -> failwithf "Unknown opcode: %A" err

let valueAt program address = program |> List.skip address |> List.head

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

let rec run pointer program =
    match program with
    | Finished p -> p
    | Running p -> 
        let i = parseInstructionAt pointer p
        let nbValuesInInstruction = 4
        runSingleInstruction p i
        |> (run (pointer + nbValuesInInstruction))

let programWithInput (noun,verb) p =
    let a :: b :: c :: ps = p
    Running (a :: noun :: verb :: ps)

let runCombinationsUntil expectedOutput program =
    [ for noun in [0..99] do
      for verb in [0..99] -> (noun, verb)]
    |> Seq.map (fun (noun, verb) -> (noun,verb), run 0 (programWithInput (noun,verb) program))
    |> Seq.find(fun (_,(result :: _)) -> result = expectedOutput)
    |> (fun ((noun,verb),_) -> (noun,verb))

printf "Testing..."

test <@ parseInstructionAt 0 [1;9;10;3;2;3;11;0;99;30;40;50] = Add(9,10,3) @>
test <@ parseInstructionAt 4 [1;9;10;3;2;3;11;0;99;30;40;50] = Multiply(3,11,0) @>
test <@ runSingleInstruction [1;9;10;3;2;3;11;0;99;30;40;50] (Add(9,10,3)) = Running [1; 9; 10; 70; 2; 3; 11; 0; 99; 30; 40; 50] @>

test <@ Running [1;9;10;3;2;3;11;0;99;30;40;50] |> run 0 = [3500; 9; 10; 70; 2; 3; 11; 0; 99; 30; 40; 50]  @>

printfn "..done!"

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : Program = input.Split([|','|]) |> Seq.map int |> Seq.toList

let expected = 19690720
runCombinationsUntil expected p //(60, 86)