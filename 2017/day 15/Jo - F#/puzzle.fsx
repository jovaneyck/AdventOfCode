#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Generator = { Value : uint64; Factor : uint64 }

let buildA start = { Value = start; Factor = 16807UL }
let buildB start = { Value = start; Factor = 48271UL }

let next generator =
    let nextValue = (generator.Value * generator.Factor) % 2147483647UL
    {generator with Value = nextValue}

let run max start =
    let lowest16BitsMask = 65536UL
    [1..max]
    |> Seq.fold 
        (fun (count, (a,b)) _ -> 
            let (nA, nB) = (next a, next b)
            let nCount = count + (if nA.Value % lowest16BitsMask = (nB.Value % lowest16BitsMask) then 1 else 0)
            nCount, (nA, nB)) 
        (0, start)

let example = (buildA 65UL, buildB 8921UL)
run 5 example

let threshold = 40_000_000

//run threshold example

//Generator A starts with 512
//Generator B starts with 191
let start = (buildA 512UL, buildB 191UL)
run threshold start