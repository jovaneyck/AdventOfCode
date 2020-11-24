#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

// Does not scale. Like, AT ALL.
//let part2 = elementAfter 0 (run stepSize 50_000_000).Buffer
//Let's see what those buffers look like after a couple of iterations

//let stepSize = 303
//run stepSize 1000
//|> Seq.zip (Seq.initInfinite id)
//|> Seq.skip 1
//|> Seq.iter (fun (idx, state) -> printfn "%d: %A" idx state.Buffer.[1])
(*
    0:  [0]
    1:  [0; 1]
    2:  [0; 2; 1]
    3:  [0; 2; 3; 1]
    4:  [0; 2; 4; 3; 1]
    5:  [0; 5; 2; 4; 3; 1]
    6:  [0; 5; 2; 4; 3; 6; 1]
    7:  [0; 7; 5; 2; 4; 3; 6; 1]
    8:  [0; 8; 7; 5; 2; 4; 3; 6; 1]
    9:  [0; 8; 7; 5; 2; 4; 3; 6; 9; 1]
    10: [0; 8; 10; 7; 5; 2; 4; 3; 6; 9; 1]
    11: [0; 8; 10; 7; 5; 2; 4; 3; 6; 11; 9; 1]
    12: [0; 12; 8; 10; 7; 5; 2; 4; 3; 6; 11; 9; 1]
    13: [0; 12; 8; 10; 7; 5; 13; 2; 4; 3; 6; 11; 9; 1]
    14: [0; 12; 14; 8; 10; 7; 5; 13; 2; 4; 3; 6; 11; 9; 1]
    15: [0; 12; 14; 8; 10; 7; 15; 5; 13; 2; 4; 3; 6; 11; 9; 1]
    16: [0; 12; 14; 8; 10; 7; 16; 15; 5; 13; 2; 4; 3; 6; 11; 9; 1]
    17: [0; 12; 14; 8; 17; 10; 7; 16; 15; 5; 13; 2; 4; 3; 6; 11; 9; 1]
    18: [0; 12; 18; 14; 8; 17; 10; 7; 16; 15; 5; 13; 2; 4; 3; 6; 11; 9; 1]
    19: [0; 12; 19; 18; 14; 8; 17; 10; 7; 16; 15; 5; 13; 2; 4; 3; 6; 11; 9; 1]
    20: [0; 12; 19; 18; 14; 8; 20; 17; 10; 7; 16; 15; 5; 13; 2; 4; 3; 6; 11; 9; 1]
*)
//Only need to keep track of the second element in the list?

type State = { Location : int; BufferLength : int; ElementAtIndex1 : int option}

let insertNumber step state number = 
    let newLocation = ((state.Location + step) % state.BufferLength) + 1
    { 
        Location = newLocation
        BufferLength = state.BufferLength + 1
        ElementAtIndex1 = 
            if newLocation = 1 then 
                Some number
            else
                state.ElementAtIndex1
    }

let run step times = 
    let start = { Location = 0; BufferLength = 1; ElementAtIndex1 = None }
    [1 .. times] //ouch. could rework this to a seq {} expression so you don't have to keep a list of 50.000.000 elements in mem just to loop
    |> List.fold (insertNumber step) start

let part2 = run 303 50_000_000