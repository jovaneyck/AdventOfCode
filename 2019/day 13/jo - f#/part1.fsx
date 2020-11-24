#load "IntCode.fsx"
open IntCode
//IntCode.t ()

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : IntCode.Program = input.Split([|','|]) |> Seq.map int64 |> Seq.toList
let o = IntCode.run (IntCode.initState [] p) |> (fun s -> s.output |> List.rev)
o |> List.chunkBySize 3 |> List.map (fun [_;_;o] -> o) |> List.filter (fun o -> o = 2L) |> List.length