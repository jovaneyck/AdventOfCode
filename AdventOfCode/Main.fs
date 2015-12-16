module App

open System
open HearSay

[<EntryPoint>]
let main args = 
    printfn "Go!"
    let result = repeatHearSay 50 "1113122113" |> String.length
    printfn "The magic number: %d" result
    Console.ReadKey() |> ignore
    0