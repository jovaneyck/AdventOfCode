#r @".\lib\Unquote.dll"
open Swensen.Unquote

open System
open System.Security.Cryptography

let hasher = MD5.Create()
let hash (word : string) =
    let bytes = System.Text.Encoding.ASCII.GetBytes(word)
    let hash = hasher.ComputeHash(bytes)
    BitConverter.ToString(hash).Replace("-", "")

let startsWith prefix (word : string) = 
    word.StartsWith(prefix)

let nth n (word : string) = 
    word.[n - 1]

let indexes = 
    Seq.initInfinite id

let appendTo word suffix = word + suffix

let debug stream = 
    stream
    |> Seq.map (fun x-> printfn "%A" x; x)

let findCode doorID passwordLength =
    indexes
    |> Seq.map (string >> appendTo doorID >> hash)
    |> Seq.filter (startsWith "00000")
    |> debug
    |> Seq.map (nth 6)
    |> Seq.take passwordLength
    |> Seq.map string
    |> String.concat ""
    
printfn "Testing..."
test <@ hash  "abc3231929" |> startsWith "00000" @>
test <@ '1' = nth 6 "00000155F8105DFF7F56EE10FA9B9ABD" @>
test <@ [0;1;2;3] = (indexes |> Seq.take 4 |> Seq.toList) @>
test <@ "1" = findCode "abc" 1 @>
//test <@ "18f47a30" = findCode "abc" 8 @> //Not the fastest kid on the block :)
printfn "Done!"

let sw = System.Diagnostics.Stopwatch.StartNew()
let secret = "reyedfim"
let solution = findCode secret (Seq.length secret)
printfn "It took me %As. to find %s" sw.Elapsed.TotalSeconds <| solution.ToLower() //30s. Meh.
