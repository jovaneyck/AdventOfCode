module CircuitsTests

open Xunit
open Swensen.Unquote

open Circuits

[<Fact>]
let simpleInputs() =
    test <@ calculateAllSignalsFor "123 -> x" = [("x", 123us)] @>
    test <@ calculateAllSignalsFor "NOT 1 -> x" = [("x", 65534us)] @>
    test <@ calculateAllSignalsFor "1 OR 2 -> x" = [("x", 3us)] @>
    test <@ calculateAllSignalsFor "6 AND 2 -> x" = [("x", 2us)] @>
    test <@ calculateAllSignalsFor "1 LSHIFT 2 -> x" = [("x", 4us)] @>
    test <@ calculateAllSignalsFor "8 RSHIFT 2 -> x" = [("x", 2us)] @>