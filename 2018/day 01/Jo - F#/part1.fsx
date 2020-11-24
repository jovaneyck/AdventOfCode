let input = System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "\input.txt")

let parse (input : string) = (input.[0], System.Int32.Parse input.[1..])
let parsed = input |> Seq.map parse

let folder acc operation =
    match operation with
    | ('+', n)-> acc + n
    | ('-', n) -> acc - n

Seq.fold folder 0 parsed