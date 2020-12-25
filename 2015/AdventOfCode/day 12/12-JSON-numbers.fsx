#load "12-JsonNumbersInput.fs"
open System.Text.RegularExpressions

let x = 
    Regex.Split(JsonNumbersInput.input, "[^\-0-9.]")
    |> List.ofArray
    |> List.filter ((<>) "")
    |> List.map (fun nb -> System.Int32.Parse(nb))
    |> List.sum

#r "./bin/Debug/FSharp.Data.dll"
open FSharp.Data

#load "12-JSON-numbers-without-red.fs"
open JSONNumbersWithoutRed

JsonNumbersInput.input |> parse |> allNonRedNumbers |> List.sum