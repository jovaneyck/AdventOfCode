#r "nuget: Unquote"
open Swensen.Unquote

//Not bothering with parsing input this time, hardcoding the instruction sets
type State = A | B | C | D | E | F
type Value = I | O
type Direction = Left | Right
type Tape = Map<int,Value>

type BasicInstruction =
    | Write of Value
    | Move of Direction
    | ContinueWith of State
type Sequence = BasicInstruction list
type ConditionalInstruction = { IfZero : Sequence; IfOne : Sequence }

type Program = Map<State, ConditionalInstruction>
type Computer = { program : Program; pointer : int; tape : Tape; state : State }

let read tape pointer = tape |> Map.tryFind pointer |> fun o -> defaultArg o O
let write pointer value tape = tape |> Map.add pointer value

let runInstruction computer instruction = 
    match instruction with
    | Write v -> { computer with tape = computer.tape |> write computer.pointer v}
    | Move Left -> { computer with pointer = computer.pointer - 1 }
    | Move Right -> { computer with pointer = computer.pointer + 1 }
    | ContinueWith s -> { computer with state = s }

let tick (computer : Computer) =
    let conditional = computer.program |> Map.find computer.state
    let current = read computer.tape computer.pointer
    let instructions = 
        match current with
        | O -> conditional.IfZero
        | I -> conditional.IfOne
    instructions |> Seq.fold runInstruction computer

let rec run (computer : Computer) nbsteps = 
    if nbsteps = 0
    then computer
    else 
        let next = tick computer
        run next (nbsteps - 1)

let checksum (tape : Tape) =
    tape |> Map.toList |> List.map snd |> List.filter ((=) I) |> List.length

let example = 
    { program = [
        (A, { IfZero = [Write I; Move Right; ContinueWith B]; IfOne = [Write O; Move Left; ContinueWith B]})
        (B, { IfZero = [Write I; Move Left; ContinueWith A]; IfOne = [Write I; Move Right; ContinueWith A]})
        ] |> Map.ofList
      pointer = 0
      state = A
      tape = Map.empty }

let input = 
    { program = [
        (A, { IfZero = [Write I; Move Right; ContinueWith B]; IfOne = [Write O; Move Right; ContinueWith F]})
        (B, { IfZero = [Write O; Move Left; ContinueWith B]; IfOne = [Write I; Move Left; ContinueWith C]})
        (C, { IfZero = [Write I; Move Left; ContinueWith D]; IfOne = [Write O; Move Right; ContinueWith C]})
        (D, { IfZero = [Write I; Move Left; ContinueWith E]; IfOne = [Write I; Move Right; ContinueWith A]})
        (E, { IfZero = [Write I; Move Left; ContinueWith F]; IfOne = [Write O; Move Left; ContinueWith D]})
        (F, { IfZero = [Write I; Move Right; ContinueWith A]; IfOne = [Write O; Move Left; ContinueWith E]})
        ] |> Map.ofList
      pointer = 0
      state = A
      tape = Map.empty }

let part1 = (run input 12964419).tape |> checksum

printf "Testing..."
(run example 6).tape |> checksum =! 3
printfn "..done"
