module App

open System

[<EntryPoint>]
let main args = 
    printfn "Go!"
    Console.ReadKey() |> ignore
    0