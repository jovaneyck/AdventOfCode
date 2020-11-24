let input = System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "\input.txt")

let parse (input : string) = (input.[0], System.Int32.Parse input.[1..])
let parsed = input |> Seq.map parse

let folder state operation =
    let (acc,visited,_) = state
    let next = 
        match operation with
        | ('+', n)-> acc + n
        | ('-', n) -> acc - n
    if visited |> Set.contains next then
        (next, visited, Some next)
    else
        (next, Set.add next visited, None)

let loop = Seq.initInfinite (fun _ -> parsed) |> Seq.concat
let frequencies = Seq.scan folder (0, Set.empty, None) loop
frequencies |> Seq.choose (fun (_,_,aDuplicate) -> aDuplicate) |> Seq.head