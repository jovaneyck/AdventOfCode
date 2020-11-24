open System.Text.RegularExpressions

type Instruction = { prerequisites : char Set; step : char }
let parse line =
    let pattern = "Step (.) must be finished before step (.) can begin."
    let m = Regex.Match(line, pattern)
    {   prerequisites = Set.singleton (char <| m.Groups.[1].Value)
        step = char <|m.Groups.[2].Value }

let bundle instructions instruction =
    match instructions |> Map.tryFind instruction.step with
    | Some prereqs -> 
        instructions 
        |> Map.add instruction.step (Set.union instruction.prerequisites prereqs)
    | None -> 
        instructions 
        |> Map.add instruction.step instruction.prerequisites

let toInstructions (prerequisiteMap : Map<char,char Set>) =
    prerequisiteMap
    |> Map.toList
    |> List.map (fun entry -> {step = fst entry; prerequisites = snd entry})

let buildDependencyGraph instructions = 
    let stepMap = 
        instructions 
        |> List.collect (fun i -> Set.union i.prerequisites (Set.singleton i.step) |> Set.toList)
        |> List.map (fun s -> (s, Set.empty))
        |> Map.ofList

    instructions
    |> List.fold bundle stepMap
    |> toInstructions

let removePrerequisite prerequisite instruction =
    { instruction with prerequisites = instruction.prerequisites |> Set.remove prerequisite }

let rec follow (acc : string) (graph : Instruction list) =
    match graph with
    | [] -> acc |> Seq.rev |> Seq.map string |> String.concat ""
    | _ ->
        let nextInstruction = 
            graph
            |> Seq.filter (fun i -> i.prerequisites |> Set.isEmpty)
            |> Seq.sortBy (fun i -> i.step)
            |> Seq.head
        let updatedGraph =
            graph
            |> List.except [nextInstruction]
            |> List.map (removePrerequisite nextInstruction.step)
        follow (string nextInstruction.step + acc) updatedGraph


let solve input = 
    let graph =
        input
        |> List.map parse
        |> buildDependencyGraph
    follow "" graph

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let example = [
    "Step C must be finished before step A can begin."
    "Step C must be finished before step F can begin."
    "Step A must be finished before step B can begin."
    "Step A must be finished before step D can begin."
    "Step B must be finished before step E can begin."
    "Step D must be finished before step E can begin."
    "Step F must be finished before step E can begin."
]

printf "Testing..."
test <@ parse "Step C must be finished before step A can begin." = { prerequisites = Set.singleton 'C'; step = 'A' } @>
test <@ solve example = "CABDFE" @>
printfn "..done!"

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Seq.toList
solve input