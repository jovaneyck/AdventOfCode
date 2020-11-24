#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Program = int list
type state = 
    | Running of Program
    | Finished of Program
type Operation = 
    | Add of int * int * int
    | Multiply of int * int * int
    | Halt

let parseInstructionAt index program =
    match program |> List.skip index with
    | 99 :: _ -> Halt
    | 1 :: a :: b :: r :: _ -> Add (a,b,r)
    | 2 :: a :: b :: r :: _ -> Multiply (a,b,r)
    | err -> failwithf "Unknown opcode: %A" err

let valueAt program index = program |> List.skip index |> List.head

let rec store value index program =
    match program with
    | x :: xs when index = 0 -> value :: xs
    | x :: xs -> x :: (store value (index - 1) xs)
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

let rec run index program =
    match program with
    | Finished p -> p
    | Running p -> 
        let i = parseInstructionAt index p
        runSingleInstruction p i
        |> (run (index + 4))

printf "Testing..."

test <@ parseInstructionAt 0 [1;9;10;3;2;3;11;0;99;30;40;50] = Add(9,10,3) @>
test <@ parseInstructionAt 4 [1;9;10;3;2;3;11;0;99;30;40;50] = Multiply(3,11,0) @>
test <@ runSingleInstruction [1;9;10;3;2;3;11;0;99;30;40;50] (Add(9,10,3)) = Running [1; 9; 10; 70; 2; 3; 11; 0; 99; 30; 40; 50] @>

test <@ Running [1;9;10;3;2;3;11;0;99;30;40;50] |> run 0 = [3500; 9; 10; 70; 2; 3; 11; 0; 99; 30; 40; 50]  @>

printfn "..done!"

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : Program = input.Split([|','|]) |> Seq.map int |> Seq.toList

let tweaked =
    let a :: b :: c :: ps = p
    a :: 12 :: 2 :: ps
run 0 (Running tweaked)