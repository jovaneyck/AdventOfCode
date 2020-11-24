#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let fuel mass =
    mass / 3 - 2

let rec fuelRequirement acc mass =
    let f = fuel mass
    if f <= 0 then acc
    else fuelRequirement (acc + f) f

let totalFuel masses =
    masses
    |> Seq.sumBy (fuelRequirement 0)

printf "Testing..."

test <@ fuel 12 = 2 @>
test <@ fuel 14 = 2 @>
test <@ fuel 1969 = 654 @>
test <@ fuel 100756 = 33583 @>

test <@ fuelRequirement 0 14 = 2 @>
test <@ fuelRequirement 0 1969 = 966 @>
test <@ fuelRequirement 0 100756 = 50346 @>

test <@ totalFuel [14; 1969] = 2 + 966 @>

printfn "..done!"

let input = System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "\input.txt")

input
|> Seq.map int
|> totalFuel