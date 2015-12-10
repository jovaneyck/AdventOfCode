module CircuitsTests

open Xunit
open Swensen.Unquote

open Circuits

[<Fact>]
let worldIsSane() =
    test <@ calculateAllSignalsFor "123 -> x" = [("x", 123us)] @>