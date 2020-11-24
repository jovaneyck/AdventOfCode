#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let nextIndex banks index =
    let length = banks |> Map.count
    (index + 1) % length

let redistribute banks = 
    let (maxIndex, max) = banks |> Map.toList |> List.maxBy snd
    let start = nextIndex banks maxIndex
    let maxTaken = banks |> Map.add maxIndex 0
    //printfn "Max is %d and it is at %d, the map looks like %A" max maxIndex maxTaken
    [1..max]
    |> List.fold 
        (fun (index, banks) _ -> 
            let value = banks |> Map.find index
            let updated = banks |> Map.add index (value + 1)
            let next = (nextIndex banks index)
            //printfn "So I added 1 to %d on index %d, resulting in %A, next index is %d" value index updated next
            (next, updated)) 
        (start, maxTaken)
    |> snd

let redistributeUnilLoop banks = 
    let b = banks |> List.indexed |> Map.ofList
    Seq.initInfinite id
    |> Seq.scan (fun banks _ -> redistribute banks) b
    |> Seq.scan 
        (fun acc redistribution -> 
            match acc with
            | None, distributions ->
                if distributions |> List.contains redistribution 
                then (Some distributions, distributions)
                else (None, redistribution :: distributions)
            | found -> found)
        (None, [])
    //|> (fun x -> printfn "%A" x; x)
    |> Seq.skipWhile (fun (found, _) -> found |> Option.isNone)
    |> Seq.head
    |> (function | (Some result, _) -> result |> Seq.length)

test <@ (redistribute ([0;2;7;0] |> List.indexed |> Map.ofList)) |> Map.toList |> List.map snd = [2;4;1;2] @>
test <@ redistributeUnilLoop [0;2;7;0] = 5 @>

let input = [5;1;10;0;1;7;13;14;3;12;8;10;7;12;0;6]
//redistributeUnilLoop input

//part2
let distributionsUntilLoop banks = 
        let b = banks |> List.indexed |> Map.ofList
        Seq.initInfinite id
        |> Seq.scan (fun banks _ -> redistribute banks) b
        |> Seq.scan 
            (fun acc redistribution -> 
                match acc with
                | None, distributions ->
                    if distributions |> List.contains redistribution 
                    then (Some (redistribution :: distributions), [])
                    else (None, redistribution :: distributions)
                | found -> found)
            (None, [])
        //|> (fun x -> printfn "%A" x; x)
        |> Seq.skipWhile (fun (found, _) -> found |> Option.isNone)
        |> Seq.head
        |> (function | (Some result, _) -> result)

let sizeOfLoop banks = 
    let redistributions = distributionsUntilLoop banks |> List.rev
    let last = redistributions |> List.last
    let lastIndex = (redistributions |> List.length) - 1
    let firstIndex = redistributions |> List.findIndex (fun el -> el = last)
    lastIndex - firstIndex
sizeOfLoop input
test <@ sizeOfLoop [0;2;7;0] = 4 @>