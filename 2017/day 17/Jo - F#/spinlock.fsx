#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type State = { Location : int; Buffer : int list }

let insertInto index number list = 
    let (before, after) = 
        if index = (List.length list) then
            list, []
        else
            List.splitAt index list
    before @ [number] @ after

let insertNumber step state number = 
    if number % 1000 = 0 then printfn "Inserting %d" number
    let length = List.length state.Buffer
    let newLocation = ((state.Location + step) % length) + 1
    { Location = newLocation; Buffer = insertInto newLocation number state.Buffer}

let run step times = 
    let start = { Location = 0; Buffer = [0] }
    [1 .. times]
    |> List.fold (insertNumber step) start

let elementAfter nb list =
    list
    |> List.skipWhile ((<>) nb)
    |> List.item 1

printf "Testing..."
test <@ run 3 0 =  { Location = 0; Buffer = [0] } @>
test <@ run 3 1 =  { Location = 1; Buffer = [0;1] } @>
test <@ run 3 2 =  { Location = 1; Buffer = [0;2;1] } @>
test <@ run 3 3 =  { Location = 2; Buffer = [0;2;3;1] } @>
test <@ run 3 9 =  { Location = 1; Buffer = [0;9;5;7;2;4;3;8;6;1] } @>

test <@ elementAfter 3 [2;3;1] = 1 @>

test <@ elementAfter 2017 (run 3 2017).Buffer = 638 @>
printfn "..done!"

let stepSize = 303
let part1 = elementAfter 2017 (run stepSize 2017).Buffer