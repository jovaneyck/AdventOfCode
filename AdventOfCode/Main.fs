﻿module App

open System
open RewriteMedicine

[<EntryPoint>]
let main args = 
    printfn "Go!"

    let generation = medicineFoundInGeneration 0 [parsedInput]
    printfn "Found the medicine in generation %i!" generation
    Console.ReadKey() |> ignore
    0