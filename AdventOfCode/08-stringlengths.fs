module StringLengths

open Xunit
open Swensen.Unquote


open System
open StringlengthsInput

type Line = | Line of string

let lineLength (Line l) = l.Length

let parse (rawInput : string) = 
    rawInput.Split('\n')
    |> List.ofArray
    |> List.map (fun line -> Line line)


let trimQuotationMarks (Line l) = 
    Line (l.Substring(1, l.Length - 2))

let rec unsescapeCharacters = function
    | [] -> []
    | '\\' :: '\\' :: t -> '\\' :: (unsescapeCharacters t)
    | '\\' :: '"' :: t -> '"' :: (unsescapeCharacters t)
    | '\\' :: 'x' :: _ :: _ :: t -> '$' :: (unsescapeCharacters t)
    | char :: t -> char :: (unsescapeCharacters t)

let unescaped (Line l)  =
    [for c in l -> c]
    |> unsescapeCharacters
    |> String.Concat
    |> Line

let inMemoryLength line = 
    line
    |> trimQuotationMarks
    |> unescaped
    |> lineLength


let rec myencode (chars : char list)
    = match chars with
        | [] -> []
        | '"' :: t -> '\\' :: '\"' :: (myencode t)
        | '\\' :: t -> '\\' :: '\\' :: (myencode t)
        | h :: t -> h :: (myencode t)

let encoded (Line l) : Line = 
    l.ToCharArray()
    |> List.ofArray
    |> myencode
    |> String.Concat
    |> Line

let encodedLength line =
    line
    |> encoded
    |> lineLength
    |> (fun length -> length + 2) //count the (implicit) quotes again?

let calculateLenghtsFor input =
    input
    |> List.map (fun line -> Line line)
    |> List.map (fun l -> (l, lineLength l, inMemoryLength l, encodedLength l))

[<Fact>]
let canParseInputToLines() =
    test <@ parse "\"a\nb\\" = [Line "\"a"; Line "b\\"] @>

[<Theory>]
[<InlineData(@"""""", 2)>]
[<InlineData(@"""abc""", 5)>]
[<InlineData(@"""aaa\""aaa""", 10)>]
[<InlineData(@"""\x27""", 6)>]
[<InlineData(@"""v\xfb\""lgs\""kvjfywmut\x9cr""", 28)>]
let stringLengthOfASingleLine (input, expectedLength) =
    test <@ lineLength (Line input) = expectedLength @>

[<Fact>]
let trimsQuotationMarks() = 
    test <@ trimQuotationMarks (Line @"""hey""") = (Line "hey") @>

[<Theory>]
[<InlineData("no_escaping_necessary", "no_escaping_necessary")>]
[<InlineData(@"\""test\""", @"""test""")>]
[<InlineData(@"\\test\\",  @"\test\")>]
[<InlineData(@"abc\x12abc",  @"abc$abc")>]
let unescapesKnownEscapeSequences(input, expectedOutput) =
    test <@ unescaped (Line input) = (Line expectedOutput) @>

[<Theory>]
[<InlineData(@"""""", 0)>]
[<InlineData(@"""abc""", 3)>]
[<InlineData(@"""aaa\""aaa""", 7)>]
[<InlineData(@"""\x27""", 1)>]
[<InlineData(@"""\x27a""", 2)>]
[<InlineData(@"""v\xfb\""lgs\""kvjfywmut\x9cr""", 18)>]
let inMemoryLengthOfASingleLine (input, expectedLength) =
    test <@ expectedLength = inMemoryLength (Line input) @>

[<Theory>]
[<InlineData(@"""""", @"\""\""")>]
[<InlineData(@"""", @"\""")>]
[<InlineData(@"""abc""", @"\""abc\""")>]
[<InlineData(@"\""", @"\\\""")>]
[<InlineData(@"""\x27""", @"\""\\x27\""")>]
[<InlineData(@"""aaa\""aaa""", @"\""aaa\\\""aaa\""")>]
let encodesAStringCorrectly(input, expectedOutput) =
    test <@ encoded (Line input) = Line expectedOutput @>

[<Fact>]
let acceptanceTest() =
    (*
    "" encodes to "\"\"", an increase from 2 characters to 6.
    "abc" encodes to "\"abc\"", an increase from 5 characters to 9.
    "aaa\"aaa" encodes to "\"aaa\\\"aaa\"", an increase from 10 characters to 16.
    "\x27" encodes to "\"\\x27\"", an increase from 6 characters to 11.
    *)
    let actual = 
        calculateLenghtsFor [@""""""; @"""abc"""; @"""aaa\""aaa"""; @"""\x27"""]
        |> List.map (fun (_, string, _, encoded) -> encoded - string)
        |> List.sum
    test <@ 19 = actual @>