module JSONNumberTests

open JSONNumbersWithoutRed
open Xunit
open Swensen.Unquote
open FSharp.Data

[<Fact>]
let canParseJson() =
    test <@ (@" {""a"" : ""b""} " |> parse) = JsonValue.Record [|("a", JsonValue.String("b"))|] @>
    test <@ (@" [""b""] " |> parse) = JsonValue.Array [| JsonValue.String("b") |] @>

[<Fact>]
let filtersOutOnlyNonRedNumbers() =
    test <@ JsonValue.String "a" |> allNonRedNumbers = [] @>
    test <@ JsonValue.Number 3m |> allNonRedNumbers = [3m] @>
    test <@ JsonValue.Array [| JsonValue.Number 3m; JsonValue.String "something" |] |> allNonRedNumbers = [3m] @>
    test <@ JsonValue.Record [|("amount", JsonValue.Number 5m)|] |> allNonRedNumbers = [5m] @>
    test <@ JsonValue.Record [|
                ("color", JsonValue.String "red")
                ("amount", JsonValue.Number 5m)|] |> allNonRedNumbers = [] @>    
    test <@ JsonValue.Record [|
                ("color", JsonValue.String "red")
                ("child", JsonValue.Record [|
                    ("amount", JsonValue.Number 5m)|])|] |> allNonRedNumbers = [] @>    
    test <@ JsonValue.Record [|
                ("color", JsonValue.String "blue")
                ("child", JsonValue.Record [|
                    ("amount", JsonValue.Number 5m)|])|] |> allNonRedNumbers = [5m] @>

[<Fact>]
let exerciseResult() =
        test <@ (JsonNumbersInput.input |> parse |> allNonRedNumbers |> List.sum) = 96852m @>