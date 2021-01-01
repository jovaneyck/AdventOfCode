#r "nuget: Unquote"
open Swensen.Unquote

//Okay, today was too hard for me (tm), not even going to try to implement the greedy search alternative
(*
The solution
My input file had:

295 elements in total
 68 were Rn and Ar (the `(` and `)`)
  7 were Y (the `,`)
Plugging in the numbers:

295 - 68 - 2*7 - 1 = 212
*)

let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example-part2.txt"
let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

type Token = string
type Replacement = { source : Token; result : Token list }

let tokenize (chars : char list) : Token list = 
    let rec tokenize acc (chars : char list) = 
        match chars with
        | [] -> acc |> List.rev
        | a :: b :: bs when System.Char.IsLower b -> tokenize ((sprintf "%c%c" a b) :: acc) bs
        | a::bs -> tokenize ((string a) :: acc) bs
    tokenize [] chars

let parseReplacement (line : string) =
    let parts = line.Split([|" => "|], System.StringSplitOptions.None)
    { source = parts.[0]; result = parts.[1] |> Seq.toList |> tokenize }

let parse lines =
    let rules = lines |> Seq.takeWhile ((<>) "") |> Seq.map parseReplacement |> Seq.toList
    let medicine = lines |> Seq.skip ((rules |> Seq.length) + 1) |> Seq.head |> Seq.toList |> tokenize
    (rules,medicine)

let (_,m) = parse input
let total = m |> Seq.length
let parens = m |> Seq.filter (fun token -> token = "Ar" || token = "Rn") |> Seq.length
let commas = m |> Seq.filter ((=) "Y") |> Seq.length
let part2 = total - parens - (2 * commas) - 1

//Behold, my own brute-force attempt that burned my cpu at generation 9 already:
//how many distinct molecules can we generate by performing 1 replacement?
let apply rule medicine =
    let rec apply acc rule med =
        match med with
        | [] -> 
            Set.difference acc (set [medicine])
        | m :: ms -> 
            if rule.source = m then
                //apply the rule and stop or don't apply the rule and continue
                let applied = (acc |> Set.map (fun med -> med @ rule.result @ ms))
                let notApplied = (acc |> Set.map (fun med -> med @ [m]))
                let recursive = apply notApplied rule ms
                Set.union applied recursive
            else
                let next = acc |> Set.map (fun med -> med @ [m])
                apply next rule ms
    apply (Set.singleton []) rule medicine

let nextGeneration rules medicines =
    medicines
    |> Set.map (fun medicine -> rules |> Set.ofList |> Set.map (fun r -> apply r medicine) |> Set.unionMany)
    |> Set.unionMany

let find rules medicine =
    let ng = nextGeneration rules 
    let seed : Set<Token list> = set [["e"]]
    let medlength = medicine |> Seq.length
    let prune (candidates : Set<Token list>) = 
        candidates |> Set.filter (fun med -> med |> Seq.length <= medlength)

    let rec find generation candidates =
        printfn 
            "generation %d has %d candidates of max length %d. We're looking for a medicine of length %d" 
            generation 
            (candidates |> Seq.length) 
            (candidates |> Seq.map Seq.length |> Seq.max) 
            medlength
        if candidates |> Set.contains medicine then generation
        else find (generation + 1) (candidates |> ng |> prune)
    find 0 seed

let (rules, medicine) = parse input

#time "on"
let part2 = find rules medicine

printf "Testing..."
test <@ parseReplacement "e => OMg" = { source = "e"; result = ["O";"Mg"]} @>
test <@ apply { source = "H"; result = ["H";"O"] } ["H"; "O"; "H"] = set [["H"; "O"; "O"; "H"];["H"; "O";"H";"O"]] @>
test <@ find 
            [{ source = "e"
               result = ["H"] }
             { source = "e"
               result = ["O"] }
             { source = "H"
               result = ["H"; "O"] }
             { source = "H"
               result = ["O"; "H"] }
             { source = "O"
               result = ["H"; "H"] }]
            ["H"; "O"; "H"] = 3 @>
test <@ find 
            [{ source = "e"
               result = ["H"] }
             { source = "e"
               result = ["O"] }
             { source = "H"
               result = ["H"; "O"] }
             { source = "H"
               result = ["O"; "H"] }
             { source = "O"
               result = ["H"; "H"] }]
            ["H"; "O";"H";"O";"H";"O"] = 6 @>
printf "..done!"