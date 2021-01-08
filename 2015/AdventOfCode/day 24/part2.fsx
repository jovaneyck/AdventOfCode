#r "nuget: Unquote"
open Swensen.Unquote

let example = [1..5] @ [7..11] |> Set.ofList
let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt" |> Seq.map int |> Set.ofSeq

//assumption: forget about the other two partitions, if we can fill 1/3 of the sleigh we assume the other two thirds even out.
let target packages = (packages |> Seq.sum) / 4
let qe packages = packages |> Seq.map uint64 |> Seq.reduce (*)

let combos (packages : int Set) (target : int) =
    let cons x xs = x :: xs
    let rec combos (packages : int Set)  (acc, runningTotal)=
        if runningTotal > target
        then []
        else if runningTotal = target 
        then acc |> List.map Set.ofSeq
        else 
            if packages |> Set.isEmpty
            then []
            else
                let p = packages |> Seq.head
                let ps = packages |> Set.remove p
                if p + runningTotal > target 
                then combos ps (acc, runningTotal)
                else
                    let withP = combos ps (acc |> List.map (cons p), runningTotal + p)
                    let withoutP = combos ps (acc, runningTotal)
                    withP @ withoutP
    combos packages ([[]], 0) |> List.distinct

let t = target input
let valids = combos input t
let candidates = valids |> List.groupBy Seq.length |> List.sortBy fst |> List.head |> snd
let entanglements = candidates |> List.map Seq.toList |> List.map qe

let part2 = entanglements |> List.min |> sprintf "%d"

printf "Testing.."
printfn "..done!"