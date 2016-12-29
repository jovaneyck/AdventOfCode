#r ".\lib\Unquote.dll"
open Swensen.Unquote

open System
open System.Text.RegularExpressions

type Marker = { take: int; repeat : int; remainder : string }
type ParseResult = { prefixLength : uint64; nextMarker : Marker option; }

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
        {prefixLength = prefix.Length |> uint64; nextMarker = nextMarker}

let withoutWhitespace (text : string) = 
    text
    |> Seq.filter (fun char -> char <> ' ')
    |> toString

//Mutually recursive functions, bye-bye tail recursion :(
let rec nibble {take = t; repeat = r; remainder = remainder} = 
    let nibbled =
        remainder
        |> Seq.take t
        |> toString
    let nibbledLength = decompressTrimmed 0UL (parse nibbled)
    let repeatedLength = (r |> uint64) * nibbledLength
    let newRemainder = 
        remainder 
        |> Seq.skip t
        |> toString
    (repeatedLength, newRemainder)
and
    decompressTrimmed acc parseResult =
        let lengthIncludingPrefix = acc + parseResult.prefixLength

        match parseResult.nextMarker with
        | None -> 
            lengthIncludingPrefix
        | Some marker -> 
            let (nibbledLength, remainderAfterNibble) = nibble marker
            decompressTrimmed (lengthIncludingPrefix + nibbledLength) (parse remainderAfterNibble)

let decompressedLength sequence = 
    sequence 
    |> withoutWhitespace
    |> parse
    |> decompressTrimmed 0UL

printfn "Testing..."
test <@ decompressedLength "(3x3)XYZ" = 9UL @>
test <@ decompressedLength "X(8x2)(3x3)ABCY" = 20UL @>
test <@ decompressedLength "(27x12)(20x12)(13x14)(7x10)(1x12)A" = 241920UL @>
test <@ decompressedLength "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" = 445UL @>
printfn "Done!"

let input = System.IO.File.ReadAllText(System.IO.Path.Combine([| __SOURCE_DIRECTORY__; "input.txt"|]))
printfn "Here we go!"
#time
input 
|> decompressedLength 
|> printfn "Decompressed length: %d"