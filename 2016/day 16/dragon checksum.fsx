#r @"..\..\0 lib\F#\Unquote.dll"
open Swensen.Unquote

let switch = not

let double word =
    let a = word
    let b = 
        a
        |> List.rev
        |> List.map switch
    a @ [false] @ b

let rec lengthen increaser maxLength word =
    if maxLength <= List.length word then
        word
    else
        lengthen increaser maxLength (increaser word)

let isEven number = number % 2 = 0

let pairs list =
    list
    |> List.pairwise
    |> List.indexed
    |> List.filter (fst >> isEven)
    |> List.map snd

let compress =
    function
    | (a,b) when a = b -> true
    | _ -> false

let rec checksum word = 
    let compressed = 
        word
        |> pairs
        |> List.map compress
    if compressed |> List.length |> isEven then
        checksum compressed
    else
        compressed

let printAndContinue msg stream =
    printfn msg;
    stream

let toBit =
    function
    | '0' -> false
    | '1' -> true
    | unknown -> failwithf "Not a binary digit: %A" unknown

let bitToString = 
    function
    | true -> "1"
    | false -> "0"

let checksumForDataOfLength l (input : string) =
    lengthen double l (input |> Seq.toList |> List.map toBit)
    |> printAndContinue "found the random data!"
    |> List.truncate l
    |> printAndContinue "truncated data!"
    |> checksum
    |> printAndContinue "checksummed it!"
    |> Seq.map bitToString
    |> String.concat ""

printfn "Testing..."
let toString = Seq.map string >> String.concat ""
let testDouble = Seq.toList >> List.map toBit >> double >> List.map bitToString >> toString
test <@ testDouble "1" = "100" @>
test <@ testDouble "0" = "001" @>
test <@ testDouble "11111" = "11111000000" @>
test <@ testDouble "111100001010" = "1111000010100101011110000" @>

test <@ lengthen (fun x -> 'a' :: x) 3 ['b'] = ['a';'a';'b'] @>

test <@ pairs [1;2;3;4] = [(1,2); (3,4)] @>

let testChecksum = Seq.toList >> List.map toBit >> checksum >> List.map bitToString >> toString
test <@ testChecksum "110010110100" = "100" @>

test <@ checksumForDataOfLength 20 "10000" = "01100" @>
printfn "Testing done!"

let input = "01000100010010111"

printfn "Cracking..."
#time
checksumForDataOfLength 35651584 input
|> printfn "The correct checksum is: %s"