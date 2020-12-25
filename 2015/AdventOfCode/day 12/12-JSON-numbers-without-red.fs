module JSONNumbersWithoutRed

open FSharp.Data
open JsonNumbersInput

//arrays
//objects
//primitive: numbers or strings

(*
Uh oh - the Accounting-Elves have realized that they double-counted everything red.

Ignore any object (and all of its children) which has any property with the value "red". Do this only for objects ({...}), not arrays ([...]).

[1,2,3] still has a sum of 6.
[1,{"c":"red","b":2},3] now has a sum of 4, because the middle object is ignored.
{"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0, because the entire structure is ignored.
[1,"red",5] has a sum of 6, because "red" in an array has no effect.
*)

let parse json = json |> JsonValue.Parse

let rec filterNumbers json : decimal option list =
    match json with
    | JsonValue.Number n -> [Some n]
    | JsonValue.Record r 
        when (r |> Array.exists (fun (key, value) -> value = JsonValue.String "red")) -> [None]
    | JsonValue.Record r -> r |> List.ofArray |> List.collect (fun (_, value) -> filterNumbers value)
    | JsonValue.Array a -> a |> List.ofArray |> List.collect (fun el -> filterNumbers el)
    | _ -> [None]

let allNonRedNumbers json = 
    json
    |> filterNumbers
    |> List.choose id