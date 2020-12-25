open System

//let exampleInput = @".#.#.#
//...##.
//#....#
//..#...
//#.#..#
//####.."
let exampleInput = @"##.#.#
...##.
#....#
..#...
#.#..#
####.#"
#load "18-GoL.fs"
open GoL

#load "18-GolInput.fs"
GoLInput.input
//exampleInput
|> parse
|> repeat 100 step
|> (fun g -> g |> printNbLightsOn; g)