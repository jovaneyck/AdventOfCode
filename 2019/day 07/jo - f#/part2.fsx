//7.2: completely immutable/functional using F# actors (MailBoxProcessors) to represent the different amplifiers.
// We run all 120 trials in parallel, where each trial models 5 "amplifier" actors in series
// and an "aggregator" actor that keep track of the running maxima.

type Pointer = int
type Program = int list
type ProgramMode = 
    | Running
    | Finished
type State = { program : Program; mode : ProgramMode; pointer : Pointer; lastOutput : int option }
type ParameterMode =
    | Position
    | Immediate
type Parameter = ParameterMode * int
type Instruction = 
    | Add of Parameter * Parameter * int
    | Multiply of Parameter * Parameter * int
    | Halt
    | Input of int
    | Output of Parameter
    | Equal of Parameter * Parameter * int
    | LessThan of Parameter * Parameter * int
    | JumpIfTrue of Parameter * Parameter
    | JumpIfFalse of Parameter * Parameter
type Message = 
    | ProcessInput of int
    | SetNextAmp of Amplifier
and Amplifier = MailboxProcessor<Message>
and AmpState = { programState : State; next : Amplifier option }
and IO = { input : unit -> Async<int>; output : int -> unit }

let initState program = { program = program; mode = Running; pointer = 0; lastOutput = None }

let zipInfinite otherInfinity one =
    otherInfinity 
    |> Seq.take (one |> Seq.length) 
    |> Seq.toList
    |> List.zip one 
    |> List.map (fun (f,s) -> (s,f))

let parseInstructionAt address program =
    let atPointer = program |> List.skip address
    let reversedInstruction = atPointer |> List.head |> string |> Seq.toList |> List.rev
    let opcode = reversedInstruction
    let modes opLength =
        let parsedModes = reversedInstruction |> List.skip opLength |> List.map (System.Char.GetNumericValue >> int) 
            
        List.append parsedModes (List.replicate 3 0)
        |> List.map (function | 0 -> Position | 1 -> Immediate)

    let parseArgs opSize nbArgs = atPointer |> List.skip 1 |> List.take nbArgs |> zipInfinite (modes opSize) 

    match opcode with
    | '9' :: '9' :: _ -> Halt
    | '8' :: '0' :: _ -> 
        let [a;b;(_,r)] = parseArgs 2 3
        Equal (a,b,r)
    | '8' :: _ -> 
        let [a;b;(_,r)] = parseArgs 1 3
        Equal (a,b,r)
    | '7' :: '0' :: _ -> 
        let [a;b;(_,r)] = parseArgs 2 3
        LessThan (a,b,r)
    | '7' :: _ -> 
        let [a;b;(_,r)] = parseArgs 1 3
        LessThan (a,b,r)
    | '6' :: '0' :: _ -> 
        let [a;b] = parseArgs 2 2
        JumpIfFalse (a,b)
    | '6' :: _ -> 
        let [a;b] = parseArgs 1 2
        JumpIfFalse (a,b)
    | '5' :: '0' :: _ -> 
        let [a;b] = parseArgs 2 2
        JumpIfTrue (a,b)
    | '5' :: _ -> 
        let [a;b] = parseArgs 1 2
        JumpIfTrue (a,b)
    | '1' :: '0' :: _ -> 
        let [a;b;(_, r)] = parseArgs 2 3
        Add (a,b,r)
    | '1' :: _ -> 
        let [a;b;(_, r)] = parseArgs 1 3
        Add (a,b,r)
    | '2' :: '0' :: _ -> 
        let [a;b;(_, r)] = parseArgs 2 3
        Multiply (a,b,r)
    | '2' :: _ -> 
        let [a;b;(_, r)] = parseArgs 1 3
        Multiply (a,b,r)
    | '4' :: '0' :: _ -> 
        let [r] = atPointer |> List.skip 1 |> List.take 1 |> zipInfinite (modes 2)
        Output r
    | '4' :: _ -> 
        let [r] = atPointer |> List.skip 1 |> List.take 1 |> zipInfinite (modes 1)
        Output r
    | '3' :: _ ->
        Input (atPointer |> List.skip 1 |> List.head)
    | err -> failwithf "Unknown opcode: %A" err

let valueAt program parameter = 
    match parameter with
    | Immediate, v -> v
    | Position, p -> program |> List.skip p |> List.head

let rec store value address program =
    match program with
    | _ :: xs when address = 0 -> value :: xs
    | x :: xs -> x :: (store value (address - 1) xs)
    | err -> failwithf "Store out of bounds: %A" err

let pointerIncrement =
    function
    | Halt -> 1
    | Input _ -> 2
    | Output _ -> 2
    | JumpIfTrue _ -> 3
    | JumpIfFalse _ -> 3
    | Add _ -> 4
    | Multiply _ -> 4
    | LessThan _ -> 4
    | Equal _ -> 4

let runSingleInstruction state instruction (io :IO) =
    async {
        let v = valueAt state.program
        match instruction with
        | Halt -> 
            return 
                { state with 
                    mode = Finished
                    pointer = state.pointer + pointerIncrement instruction }
        | Add(a,b,r) ->
            let sum = (v a) + (v b)
            return 
                { state with 
                    program = store sum r state.program
                    pointer = state.pointer + pointerIncrement instruction }
        | Multiply(a,b,r) ->
            let product = (v a) * (v b)
            return  
                { state with 
                    program = store product r state.program
                    pointer = state.pointer + pointerIncrement instruction }
        | Input r ->
            let! i = io.input ()
            return
                { state with 
                    program = store i r state.program
                    pointer = state.pointer + pointerIncrement instruction }
        | Output r ->
            let o = v r
            io.output o
            return
                { state with 
                    pointer = state.pointer + pointerIncrement instruction
                    lastOutput = Some o }
        | Equal (a,b,r) ->
            let one = v a
            let other = v b
            let result = if one = other then 1 else 0
            return
                { state with 
                    program = store result r state.program
                    pointer = state.pointer + pointerIncrement instruction }
        | JumpIfTrue (a,r) ->
            return
                { state with 
                    pointer = if v a <> 0 then v r else  state.pointer + pointerIncrement instruction }
        | JumpIfFalse (a,r) ->
            return
                { state with 
                    pointer = if v a = 0 then v r else  state.pointer + pointerIncrement instruction }
        | LessThan (a,b,r) ->
            let result = if (v a) < (v b) then 1 else 0
            return
                { state with 
                    program = store result r state.program
                    pointer = state.pointer + pointerIncrement instruction }
    }

let rec runProgram io state =
    async {
        let! s = state
        match s.mode with
        | Finished -> 
            return s
        | Running -> 
            let i = parseInstructionAt s.pointer s.program
            return! runSingleInstruction s i io |> runProgram io
    }

let rec permute l = 
  let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

  match l with
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

type AggregatorMessage = | AResult of int | GetMax of AsyncReplyChannel<int>
type Aggregator = MailboxProcessor<AggregatorMessage>
let aggregator () = MailboxProcessor.Start(fun inbox-> 
    let rec messageLoop max = async{
        let! msg = inbox.Receive()
        match msg with
        | AResult r ->
            let next = if r > max then r else max
            return! messageLoop next
        | GetMax reply -> 
            reply.Reply max
            return! messageLoop max
    }

    messageLoop -1)

let amplifier program id (aggregator : Aggregator) = MailboxProcessor.Start(fun inbox -> 
    let rec init = async {
        let! nextAmp = inbox.Scan(function | SetNextAmp n -> Some (async.Return n) | _ -> None)

        let rec run state = async {
            let input () =
                inbox.Scan (function | ProcessInput i -> Some (async.Return i) | _ -> None)
            let output o = 
                state.next |> Option.iter (fun a -> a.Post (ProcessInput o))
            let! next = runProgram ({ input = input; output = output }) (async.Return state.programState)
            if id = 'E' && next.mode = Finished 
            then 
                next.lastOutput |> Option.iter (AggregatorMessage.AResult >> aggregator.Post)
                return () 
            elif next.mode = Finished then
                return ()
            else 
                return! run { state with programState = next }
        }

        return! run { next = Some nextAmp; programState = initState program }
    }
    init)

let trial program (phases : int list) aggregator =
    let a = amplifier program 'A' aggregator
    let b = amplifier program 'B' aggregator
    let c = amplifier program 'C' aggregator
    let d = amplifier program 'D' aggregator
    let e = amplifier program 'E' aggregator
    a.Post (Message.SetNextAmp b)
    b.Post (Message.SetNextAmp c)
    c.Post (Message.SetNextAmp d)
    d.Post (Message.SetNextAmp e)
    e.Post (Message.SetNextAmp a)

    a.Post (Message.ProcessInput phases.[0])
    b.Post (Message.ProcessInput phases.[1])
    c.Post (Message.ProcessInput phases.[2])
    d.Post (Message.ProcessInput phases.[3])
    e.Post (Message.ProcessInput phases.[4])

    a.Post (Message.ProcessInput 0)

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let program : Program = input.Split([|','|]) |> Seq.map int |> Seq.toList

let candidates = ([5..9] |> permute)
let agg = aggregator ()
candidates
|> List.map (fun candidate -> async { return trial program candidate agg})
|> Async.Parallel
|> Async.RunSynchronously 
agg.PostAndReply AggregatorMessage.GetMax //30872528