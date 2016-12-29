#r ".\lib\Unquote.dll"
open Swensen.Unquote

open System
open System.Text.RegularExpressions

type Marker = { take: int; repeat : int; remainder : string }
type ParseResult = { prefix : string; nextMarker : Marker option; }

let parseInt = Int32.Parse
let toString characters = characters |> Seq.toArray |> String

let parse input = 
    let match' = Regex.Match(input, "(?<prefix>[^\(\)]*)(\((?<take>\d*)x(?<repeat>\d*)\))?(?<remainder>.*)")
    let valueOf (groupName : string) = match'.Groups.[groupName].Value
    let hasMatch (groupName : string) = match'.Groups.[groupName].Success
    match match'.Success with
    | false -> 
        failwithf "Hmm, this magical regex doesn't match on: %s" input
    | true -> 
        let prefix = valueOf "prefix"
        let nextMarker =
            if hasMatch "take" then
                let take = valueOf "take" |> parseInt
                let repeat = valueOf "repeat" |> parseInt
                let remainder = valueOf "remainder"
                Some <| {take = take; repeat = repeat; remainder = remainder}
            else
                None
        {prefix = prefix; nextMarker = nextMarker}

let withoutWhitespace (text : string) = 
    text
    |> Seq.filter (fun char -> char <> ' ')
    |> toString

let nibble {take = t; repeat = r; remainder = remainder} = 
    let toRepeat =
        remainder
        |> Seq.take t
    let repeated =
        toRepeat
        |> Seq.replicate r
        |> Seq.collect id
        |> toString
    let newRemainder = 
        remainder 
        |> Seq.skip t
        |> toString
    (repeated, newRemainder)

let rec decompressTrimmed acc parseResult =
    let includingPrefix = acc + parseResult.prefix

    match parseResult.nextMarker with
    | None -> 
        includingPrefix
    | Some marker -> 
        let (nibbled, remainderAfterNibble) = nibble marker
        decompressTrimmed (includingPrefix  + nibbled) (parse remainderAfterNibble)

let decompress sequence = 
    sequence 
    |> withoutWhitespace
    |> parse
    |> decompressTrimmed ""

printfn "Testing..."
test <@ parse "abc" = {prefix = "abc"; nextMarker = None} @>
test <@ parse "" = {prefix = ""; nextMarker = None} @>
test <@ parse "a(1x2)" = {prefix = "a"; nextMarker = Some {take = 1; repeat = 2; remainder = ""}} @>
test <@ parse "a(2x3)b" = {prefix = "a"; nextMarker = Some {take = 2; repeat = 3; remainder = "b"}} @>
test <@ parse "a(2x3)(4x5)" = {prefix = "a"; nextMarker = Some {take = 2; repeat = 3; remainder = "(4x5)"}} @>
test <@ parse "(1x10)a" = {prefix = ""; nextMarker = Some {take = 1; repeat = 10; remainder = "a"}} @>

test <@ decompress "ign ores  whitespa ce" = "ignoreswhitespace" @>
test <@ decompress "ADVENT" = "ADVENT" @>
test <@ decompress "A(1x5)BC" = "ABBBBBC" @>
test <@ decompress "(3x3)XYZ" = "XYZXYZXYZ" @>
test <@ decompress "A(2x2)BCD(2x2)EFG" = "ABCBCDEFEFG" @>
test <@ decompress "(6x1)(1x3)A" = "(1x3)A" @>
test <@ decompress "X(8x2)(3x3)ABCY" = "X(3x3)ABC(3x3)ABCY" @>
printfn "Done!"

let input = System.IO.File.ReadAllText(System.IO.Path.Combine([| __SOURCE_DIRECTORY__; "input.txt"|]))
input 
|> decompress 
|> String.length
|> printfn "Decompressed length: %d"