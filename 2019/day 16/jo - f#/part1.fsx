#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

printf "Testing..."

printfn "..done!"

let phase input =
    let base_pattern = [0;1;0;-1]
    let pattern idx =
        base_pattern
        |> Seq.collect (Seq.replicate (idx+1))
        |> Seq.replicate 200
        |> Seq.collect id
        |> Seq.skip 1
        |> Seq.take (input |> Seq.length)
        |> Seq.toList
    [ for i in 0 .. (input |> Seq.length) - 1 -> 
        pattern i
        |> List.zip input
        |> List.map (fun (i,p) -> i*p)
        |> List.sum
        |> (fun x -> x % 10)
        |> abs
    ]

let digits (s : string) =
    s.Trim() 
    |> Seq.toList 
    |> List.map (string >> int)

let rec phases n digits =
    if n = 0 then digits
    else phases (n-1) (phase digits)

let t () =
    printf "Testing..."
    test <@ "12345678" |> digits |> phases 1 = ("48226158" |> digits) @>
    test <@ "12345678" |> digits |> phases 2 = ("34040438" |> digits) @>
    test <@ "12345678" |> digits |> phases 3 = ("03415518" |> digits) @>
    test <@ "12345678" |> digits |> phases 4 = ("01029498" |> digits) @>

    test <@ "80871224585914546619083218645595" |> digits |> phases 100 |> List.take 8 = ("24176176" |> digits) @>
    test <@ "19617804207202209144916044189917" |> digits |> phases 100 |> List.take 8 = ("73745418" |> digits) @>
    test <@ "69317163492948606335995924319873" |> digits |> phases 100 |> List.take 8 = ("52432133" |> digits) @>
    printfn "...done!"
t ()

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let result = input |> digits |> phases 100 |> List.take 8