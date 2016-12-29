#r @"..\..\0 lib\F#\Unquote.dll"
open Swensen.Unquote

let rec play elves otherElves =
    //Working with two lists, as appending to end of the list sucked performance-wise. 
    //With this approach we have to reverse the list sometimes, but amortized cost O(1).
    match elves, otherElves with
    | [winner], [] -> winner
    | [], [winner] -> winner
    | taker :: giver :: other, _ -> play other (taker :: otherElves)
    | one, other -> play (one @ (other |> List.rev)) []

let solveFor nb = play [1..nb] []

test <@ solveFor 5 = 3 @>
(*
solveFor 3014603
*)

//This model-it-as-a-list approach REALLY doesn't scale for pt2's random access. 
//So let's redo it with mutable lists!
open System.Collections.Generic

let rec play2 (elves : List<int>) idx =
    if elves.Count = 1 
    then elves.[0]
    else
        let giverIdx = (idx + (elves.Count / 2)) % elves.Count
//        printfn "Elf %d taking from elf %d" elves.[idx] elves.[giverIdx]
//        printfn "Elf at %d taking from elf at %d" idx giverIdx
        elves.RemoveAt(giverIdx) //BEWARE THE MUTABLE DATA DEVIL
        let next = (if idx > giverIdx then idx else idx + 1) % elves.Count
        play2 elves next

let solvePt2 nb = play2 (List<int>([1..nb] |> List.toSeq)) 0

test <@ solvePt2 5 = 2 @>

(*
#time
solvePt2 3014603
*)