module App

open System
open StringLengths
open StringlengthsInput

[<EntryPoint>]
let main args = 
    printf "Hello!"
    realInput
    |> List.ofSeq
    |> calculateLenghtsFor
//    |> Seq.iter (fun (Line l, ll, iml) -> (printfn "WUT? Line: %s \n stringlength: %d \n inmem: %d" l ll iml))
    |> Seq.map (fun (l, string, inmem, encoded) -> (l, encoded - string))
    |> Seq.map (fun (_, delta) -> delta)
    |> Seq.sum
    |> (printfn "Total delta: %d")
//    |> (printf "Total difference between string and inmem length = %d")
    Console.ReadKey() |> ignore
    0