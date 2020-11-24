#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let fuel mass =
    mass / 3 - 2

let totalFuel masses =
    masses
    |> Seq.sumBy fuel

printf "Testing..."

test <@ fuel 12 = 2 @>
test <@ fuel 14 = 2 @>
test <@ fuel 1969 = 654 @>
test <@ fuel 100756 = 33583 @>

test <@ totalFuel [14;1969] = 656 @>

printfn "..done!"

let input = System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "\input.txt")

input
|> Seq.map int
|> totalFuel