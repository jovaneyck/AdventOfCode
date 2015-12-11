module StringLengths

open Xunit
open Swensen.Unquote

type Line = | Line of string

let stringLength (Line l) = l.Length

[<Theory>]
[<InlineData(@"""""", 2)>]
[<InlineData(@"""abc""", 5)>]
[<InlineData(@"""aaa\""aaa""", 10)>]
[<InlineData(@"""\x27""", 6)>]
let stringLengthOfASingleLine (input, expectedLength) =
    test <@ stringLength (Line input) = expectedLength @>