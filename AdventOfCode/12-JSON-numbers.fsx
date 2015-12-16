#load "12-JsonNumbersInput.fs"

open System.Text.RegularExpressions

let x = 
    Regex.Split(JsonNumbersInput.input, "[^\-0-9.]")
    |> List.ofArray
    |> List.filter ((<>) "")
    |> List.map (fun nb -> System.Int32.Parse(nb))
    |> List.sum