#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
open System.Text.RegularExpressions

let split (delim : char) (text : string) = 
    text.Split [|delim|] |> Seq.toList

type Move = 
    | Spin of int 
    | Exchange of (int * int)
    | Partner of (char * char)

let parseMove text =
    let parseInt = System.Int32.Parse
    let spin = Regex.Match(text, "s(.*)")
    let exchange = Regex.Match(text, "x(.*)/(.*)")
    let partner = Regex.Match(text, "p(.*)/(.*)")

    if spin.Success then 
        Spin <| (parseInt spin.Groups.[1].Value)
    elif exchange.Success then 
        Exchange <| (parseInt exchange.Groups.[1].Value, parseInt exchange.Groups.[2].Value)
    elif partner.Success then
        Partner <| (char partner.Groups.[1].Value, char partner.Groups.[2].Value)
    else
        failwithf "Did not recognize instruction %s" text

let parse text =
    text
    |> split ','
    |> List.map parseMove

let doSpin positions size = 
    let length = positions |> List.length
    let (h,t) = positions |> List.splitAt (length - size)
    List.append t h

let doExchange<'a> (positions : 'a list) (a,b) = 
    let programA = positions.[a]
    let programB = positions.[b]
    let rec insertInto positions (a,b) =
        match positions with
        | [] -> []
        | _ :: t when a = 0 -> programB :: (insertInto t (a-1, b-1))
        | _ :: t when b = 0 -> programA :: (insertInto t (a-1, b-1))
        | h :: t -> h :: (insertInto t (a-1, b-1))
    insertInto positions (a,b)

let doPartner positions (a,b) = 
    let indexA = positions |> List.findIndex (fun x -> x = a)
    let indexB = positions |> List.findIndex (fun x -> x = b)
    doExchange positions (indexA, indexB)

let perform positions move = 
    match move with
    | Spin s -> doSpin positions s
    | Exchange e -> doExchange positions e
    | Partner p -> doPartner positions p

let dance startPositions moves =
    moves
    |> List.fold perform startPositions

let toString = Seq.map string >> String.concat ""

//part 2
let rec repeat nb start moves =
    if nb = 0 then 
        start
    else
        repeat (nb - 1) (dance start moves) moves

//1 billion iterations makes computer go boom, in a state space this small hoping for a cycle somewhere
let rec lengthOfPossibleCycle endingPositions current moves =
    if Set.contains current endingPositions then 
        0
    else 
        1 + lengthOfPossibleCycle (Set.add current endingPositions) (dance current moves) moves

let example = "s1,x3/4,pe/b"
printf "Testing..."
test <@ parseMove "s12" = Spin 12 @>
test <@ parseMove "x12/34" = Exchange (12, 34) @>
test <@ parseMove "pe/b" = Partner ('e','b') @>
test <@ parse example = [Spin 1; Exchange (3,4); Partner ('e','b')] @>

test <@ doSpin ['a'..'e'] 3 |> toString = "cdeab" @>
test <@ doSpin ['a'..'e'] 1 |> toString = "eabcd" @>
test <@ doSpin ['a'..'e'] 5 |> toString = "abcde" @>

test <@ doExchange ['a'..'e'] (3, 4) |> toString = "abced" @>
test <@ doExchange ['a'..'e'] (0, 4) |> toString = "ebcda" @>

test <@ doPartner ['a'..'e'] ('a', 'b') |> toString = "bacde" @>
test <@ doPartner ['a'..'e'] ('e', 'c') |> toString = "abedc" @>

test <@ dance ['a'..'e'] (parse example) |> toString = "baedc" @>
test <@ repeat 2 ['a'..'e'] (parse example) |> toString = "ceadb" @>
printfn "..done"

let input = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt" )
let moves = parse input
let finalPositions = dance ['a'..'p'] moves
sprintf "%s" (toString finalPositions)

let cycle_length = lengthOfPossibleCycle Set.empty ['a'..'p'] moves
let billion = 1_000_000_000
let nb_dances = billion % cycle_length
let part2 = repeat nb_dances ['a'..'p'] moves |> toString