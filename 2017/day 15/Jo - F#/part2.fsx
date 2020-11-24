#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Generator = { Value : uint64; Factor : uint64; Criterium : uint64 }

let buildA start = { Value = start; Factor = 16807UL; Criterium = 4UL }
let buildB start = { Value = start; Factor = 48271UL; Criterium = 8UL }

let next generator =
    let nextValue = 
        Seq.initInfinite id
        |> Seq.scan (fun v _ -> (v * generator.Factor) % 2147483647UL) generator.Value 
        |> Seq.skip 1
        |> Seq.find (fun v -> v % generator.Criterium = 0UL)
    {generator with Value = nextValue}

let run max start =
    let lowest16BitsMask = 65536UL
    [1..max]
    |> Seq.fold 
        (fun (count, (a,b)) _ -> 
            let (nA, nB) = (next a, next b)
            //printfn "%A" (nA, nB)
            let nCount = count + (if nA.Value % lowest16BitsMask = (nB.Value % lowest16BitsMask) then 1 else 0)
            nCount, (nA, nB)) 
        (0, start)

let threshold = 5_000_000
let example = (buildA 65UL, buildB 8921UL)

//run threshold example

//Generator A starts with 512
//Generator B starts with 191
let start = (buildA 512UL, buildB 191UL)
//run threshold start