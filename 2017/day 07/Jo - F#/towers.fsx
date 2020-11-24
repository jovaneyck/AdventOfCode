#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
open System.Windows.Forms.VisualStyles.VisualStyleElement.ListView

let splitLines (text : string) = text.Split('\n')

let example = splitLines @"pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"

type Name = string
type Program = {name: string; weight: int; immediatelyAbove : Name list }

let parseLine line =
    let parseAbove (text : string) = 
        text.Split([|", "|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.toList

    let regex = System.Text.RegularExpressions.Regex("(.*) \((.*)\)( -> (.*))?")
    let m = regex.Match(line)
    let name = m.Groups.[1].Value
    let weight = m.Groups.[2].Value |> int
    let above = m.Groups.[4].Value
    {name = name; weight = weight; immediatelyAbove = parseAbove above}

let parse input =
    input
    |> Seq.map parseLine

//build tower
//bottom?

type AboveMap = (Program * string) seq
let buildAboveMap programs : AboveMap = 
    programs
    |> Seq.collect (fun p -> p.immediatelyAbove |> Seq.map (fun aboveName -> (p, aboveName)))

let findRoot aboveMap = 
    //root = the one that is not directly above any other programs
    let allLowerPrograms = aboveMap |> Seq.map fst
    allLowerPrograms |> Seq.find (fun p -> aboveMap |> Seq.map snd |> Seq.contains p.name |> not)


let solve input =
    let map = 
        input
        |> parse
        |> buildAboveMap
    let root = findRoot map
    root

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
solve input

type Tower = { name : string; directlyAbove : Tower list}
let buildTree rootName (map : AboveMap) =
    let rec buildRec rootName =
        let aboves = map |> Seq.filter (fun (bel, abo) -> bel.name = rootName) |> Seq.map snd
        { name = rootName; directlyAbove = aboves |> Seq.map buildRec |> Seq.toList}
    
    buildRec rootName

type WeightedTree = { name : string; weight : int; above : WeightedTree list}

let rec weigh (programs : Program seq) (tower : Tower) =
    let programWeight = (programs |> Seq.find (fun p ->  p.name = tower.name)).weight
    let weightedSubtrees = tower.directlyAbove |> List.map (weigh programs)
    let w = programWeight + (weightedSubtrees |> Seq.fold (fun acc t -> acc + t.weight) 0)
    { name = tower.name; above = weightedSubtrees; weight = w }

printf "Testing..."
test <@ parseLine "test (33)" = {name = "test"; weight = 33; immediatelyAbove = [] } @>
test <@ (parseLine "n (1) -> a, b, cd").immediatelyAbove = ["a";"b";"cd"] @>
test <@ (solve example).name = "tknk" @>
printfn "..done"

//part 1
//solve input //vtzay

//part 2
let parsed = input |> parse 
let map = parsed |> buildAboveMap
let root = "vtzay"//findRoot map
let tree = buildTree root map
let weightedTree = weigh parsed tree

let rec walk (tree : WeightedTree) =
    let subtreeweights = tree.above |> Seq.groupBy (fun t -> t.weight)
    if subtreeweights |> Seq.length <> 1 then
        let different = subtreeweights |> Seq.find (fun (_, group) -> group |> Seq.length = 1) |> snd |> Seq.head
        let otherWeights = subtreeweights |> Seq.find (fun (_, group) -> group |> Seq.length > 1) |> fst
        printfn "Node %A is unbalanced, walking down its different child %A. Other subtrees have weight %A" (tree.name, tree.weight) (different.name, different.weight) otherWeights
        walk different
    else
        printfn "Everything from here on upward is balanced"

walk weightedTree
//lnpuarm needs to slim down 1437 - 1429 = 8
let realWeight = parsed |> Seq.find (fun p -> p.name = "lnpuarm") |> (fun p -> p.weight)