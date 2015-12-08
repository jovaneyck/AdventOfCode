module Library2

open System

[<EntryPoint>]
let main args = 
    printf "Hello!"
    Console.ReadKey() |> ignore
    0