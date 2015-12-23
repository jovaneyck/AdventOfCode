module App

open System
open GoL
open GoLInput

[<EntryPoint>]
let main args = 
    printfn "Go!"

    input
    |> parse
    |> repeat 100 step
    |> (fun g -> g |> printNbLightsOn; g)
    |> ignore

    Console.ReadKey() |> ignore
    0