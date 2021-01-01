#r "nuget: Unquote"
open Swensen.Unquote

let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"
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

//how many distinct molecules can we generate by performing 1 replacement?
let apply rule medicine =
    let rec apply acc rule medicine =
        match medicine with
        | [] -> acc
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

let distinctMolecules (rules,medicine) =
    rules
    |> List.map (fun rule -> apply rule medicine)
    |> Set.unionMany

let solve ((_, medicine) as parsed) =
    parsed |> distinctMolecules |> (fun s -> Set.difference s (Set.singleton medicine)) |> Set.count

printf "Testing..."
test <@ parseReplacement "e => OMg" = { source = "e"; result = ["O";"Mg"]} @>
test <@ apply { source = "H"; result = ["H";"O"] } ["H"; "O"; "H"] = set [["H"; "O"; "H"];["H"; "O"; "O"; "H"];["H"; "O";"H";"O"]] @>
test <@ example |> parse |> solve = 4 @>
test <@ solve ([{ source = "H"; result = ["H"; "O"] }; { source = "H"; result = ["O"; "H"] }; { source = "O"; result = ["H"; "H"] }], ["H";"O";"H";"O";"H";"O";]) = 7 @>
test <@ input |> parse |> solve = 576 @>
printf "..done!"