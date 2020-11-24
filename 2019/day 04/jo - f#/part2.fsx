#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let toDigits n = 
    let rec toDigits acc n = 
        if n = 0 then acc
        else
            let d = n % 10
            let r = n / 10
            toDigits (d :: acc) r
    toDigits [] n

let rec doubles n = 
    match n with
    | [a;b] when a = b -> true
    | a :: b :: c :: _ when a = b && b <> c -> true
    | a :: ns -> doubles (ns |> List.skipWhile (fun n -> n = a))
    | [] -> false

let rec increasing n = 
    match n with
    | a :: b :: _ when a > b -> false
    | _ :: ns -> increasing ns
    | _ -> true

let valid candidates =
    candidates
    |> List.map toDigits
    |> List.filter doubles
    |> List.filter increasing

printf "Testing..."

test <@ toDigits 1123 = [1;1;2;3] @>

test <@ doubles [1;2;3] |> not @>
test <@ doubles [1;1;2]  @>
test <@ doubles [2;1;1]  @>

//new rule
test <@ doubles [1;1;1]  |> not @>
test <@ doubles [1;2;3;4;4;4] |> not @>

test <@ increasing [] @>
test <@ increasing [1] @>
test <@ increasing [1;1] @>
test <@ increasing [1;2;4] @>
test <@ increasing [1;2;1] |> not @>

printfn "..done!"

let candidates = [178416..676461]
valid candidates |> List.length