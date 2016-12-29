#r @"..\..\0 lib\F#\Unquote.dll"
open Swensen.Unquote

open System
open System.Security.Cryptography

let hasher = MD5.Create()
let hash (word : string) =
    let bytes = System.Text.Encoding.ASCII.GetBytes(word)
    let hash = hasher.ComputeHash(bytes)
    BitConverter.ToString(hash).Replace("-", "").ToLower()

let appendTo word appendage = word + appendage
let contains substring (text : string) = text.Contains(substring)

let containsNInARow times letter word =
    let repeat = letter |> String.replicate times
    word |> contains repeat

let (|NInARowOf|_|) times word = 
    word
    |> Seq.distinct
    |> Seq.map string
    |> Seq.tryFind(fun letter -> containsNInARow times letter word)

let fiveInARow letter (_,hash) = containsNInARow 5 letter hash

let validHash ((_,h), nextHashes) = 
    match h with
    | NInARowOf 3 character -> nextHashes |> Seq.exists (fiveInARow character)
    | _ -> false

let keys salt =
    Seq.initInfinite (fun idx -> (idx, (idx |> string |> appendTo salt)))
    |> Seq.map (fun (idx, word) -> (idx, hash word))
    |> Seq.windowed (1 + 1000)
    |> Seq.map (fun hashes -> (Seq.head hashes, Seq.tail hashes))
    |> Seq.filter validHash
    |> Seq.map (fst >> fst)

let stretch hash' = 
    let rec str times h =
        if times = 0 then
            h
        else
            str (times - 1) (hash h)
    str 2016 hash'

let debug stream =
 stream |> Seq.map (fun el -> printfn "%A" el; el)

let stretchedKeys salt =
    Seq.initInfinite (fun idx -> (idx, (idx |> string |> appendTo salt)))
    |> Seq.map (fun (idx, word) -> (idx, word |> hash |> stretch))
    |> Seq.windowed (1 + 1000)
    |> Seq.map (fun hashes -> (Seq.head hashes, Seq.tail hashes))
    |> Seq.filter validHash
    |> Seq.indexed
    //|> debug
    |> Seq.map (snd >> fst >> fst)

printfn "Testing..."
test <@ keys "abc" |> Seq.take 2 |> Seq.toList = [39; 92] @>
test <@ keys "abc" |> Seq.take 65 |> Seq.last = 22728 @>
test <@ stretch (hash "abc0") =  "a107ff634856bb300138cac6568c0f24" @>
test <@ stretchedKeys "abc" |> Seq.take 1 |> Seq.toList = [10] @>
printfn "Done!"

let secret = "ngcjuoqr"
//let secret = "ihaygndm"

keys secret |> Seq.take 64 |> Seq.last
printfn "Going for a stretch..."
#time
stretchedKeys secret |> Seq.take 64 |> Seq.last