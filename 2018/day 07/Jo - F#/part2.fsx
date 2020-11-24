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

let removePrerequisites prerequisites instruction =
    { instruction with prerequisites = Set.difference instruction.prerequisites (Set.ofList prerequisites) }

let duration offset step = 
    let baseIndex = int 'A'
    offset + (int step) - baseIndex + 1

type State = { result : char list; idleWorkers : int; workInProgress: (char * int) list; ticks : int; graph: Instruction list }
let tick delta state =
    let tickedWorkInProgress = state.workInProgress |> List.map (fun (c, t) -> (c, t-1))
    let (workDone, workInProgress) = tickedWorkInProgress |> List.partition (fun (_,t) -> t = 0)
    let nbFreedWorkers = workDone |> Seq.length
    let dones = workDone |> List.map fst
    let graphWithoutDones = state.graph |> List.map (removePrerequisites dones)
    let workersReadyForWork = state.idleWorkers + nbFreedWorkers
    
    let nextInstructions = 
        graphWithoutDones
        |> Seq.filter (fun i -> i.prerequisites |> Set.isEmpty)
        |> Seq.sortBy (fun i -> i.step)
        |> (fun s -> Seq.take ([workersReadyForWork; Seq.length s] |> Seq.min) s)

    let graphWithoutNextInstructions =
        graphWithoutDones
        |> List.except nextInstructions
                       
    let newWorkInProgress = 
        nextInstructions
        |> Seq.map (fun i -> (i.step, duration delta i.step))
        |> Seq.toList

    { state with
        ticks = state.ticks + 1
        idleWorkers = workersReadyForWork - (newWorkInProgress |> Seq.length)
        workInProgress = List.append workInProgress newWorkInProgress 
        graph = graphWithoutNextInstructions
        result = List.append state.result dones }

let follow (nbWorkers : int) (delta : int) (graph : Instruction list) =
    let rec f state =
        match state.graph, state.workInProgress with
        | [], [] -> (state.result |> Seq.map string |> String.concat "", state.ticks)
        | _ ->
            let newState = tick delta state
            f newState

    let initial = { result = []; idleWorkers = nbWorkers; workInProgress = []; ticks = -1; graph = graph }
    f initial

let solve nbWorkers delta input = 
    let instructions =
        input
        |> List.map parse
        |> buildDependencyGraph
    follow nbWorkers delta instructions

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
test <@ removePrerequisites ['A';'B'] { step = 'C'; prerequisites = ['A';'D'] |> Set.ofList } = { step = 'C'; prerequisites = ['D'] |> Set.ofList } @>
test <@ duration 60 'A' = 61 @>
test <@ duration 60 'Z' = 86 @>
test <@ duration 0 'A' = 1 @>
test <@ solve 2 0 example = ("CABFDE", 15) @>
printfn "..done!"

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Seq.toList
solve 5 60 input