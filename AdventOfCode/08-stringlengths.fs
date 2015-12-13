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

let calculateLenghtsFor (input : string list) =
    input
    |> List.map (fun line -> Line line)
    |> List.map (fun l -> (l, lineLength l, inMemoryLength l))

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