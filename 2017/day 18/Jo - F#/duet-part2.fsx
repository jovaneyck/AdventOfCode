type RegisterName = RegisterName of char
type Registers = Map<RegisterName, int64>
type Value = | FromRegister of RegisterName | RawValue of int64
let registerName = RegisterName
let registerValue r = FromRegister <| RegisterName r
let integerValue i = RawValue i

type Instruction = 
    | Snd of Value
    | Set of (RegisterName * Value)
    | Add of (RegisterName * Value)
    | Mul of (RegisterName * Value)
    | Mod of (RegisterName * Value)
    | Rcv of RegisterName
    | Jgz of (Value * Value)

type State = {Program : int64; NbSends : int64; Pointer : int64; Registers : Registers }

type Message = 
    | GetState of AsyncReplyChannel<State>
    | TalkTo of MailboxProcessor<Message>
    | Receive of int64
    | Start

type Channel = {Incoming : unit -> Async<Message>; Outgoing : Message -> unit}

let init id = {Program = id; NbSends = 0L; Pointer = 0L; Registers = Map.ofList [(RegisterName 'p', id)] }

let split (delims : string array) (text : string) = 
    text.Split(delims, System.StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray

let tryParseInt text = 
    text
    |> Seq.map string
    |> String.concat ""
    |> System.Int64.TryParse

let parseValue (text : char list) = 
    match tryParseInt text with
    | true, parsed -> integerValue parsed
    | _ -> registerValue (char text.[0])

let parseInstruction text =
    match text |> Seq.toList with
    | 's'::'n'::'d' :: ' ' :: freq -> Snd <| parseValue freq
    | 's'::'e'::'t' :: ' ' :: reg :: ' ' :: v -> Set <| (registerName reg, parseValue v)
    | 'a'::'d'::'d' :: ' ' :: reg :: ' ' :: v -> Add <| (registerName reg, parseValue v)
    | 'm'::'u'::'l' :: ' ' :: reg :: ' ' :: v -> Mul <| (registerName reg, parseValue v)
    | 'm'::'o'::'d' :: ' ' :: reg :: ' ' :: v -> Mod <| (registerName reg, parseValue v)
    | 'r'::'c'::'v' :: ' ' :: reg :: [] -> Rcv <| registerName reg
    | 'j'::'g'::'z' :: ' ' :: v1 :: ' ' :: v2 -> Jgz <| (parseValue [v1] (*OUCH, incorrect when multi-digit*), parseValue v2)
    | unknown -> failwithf "Failed to parse %A" unknown

let parse instructions =
    instructions
    |> split [|"\n"; "\r"|]
    |> List.map parseInstruction

let getRegisterValue register (registers : Registers) =
    registers
    |> Map.tryFind register
    |> (fun o -> defaultArg o 0L)

let getValue v registers = 
    match v with
    | FromRegister registerName -> getRegisterValue registerName registers
    | RawValue rawValue -> rawValue

let setValue register v state = 
    let newRegisters =
        state.Registers 
        |> Map.add register v
    { state with Registers = newRegisters }

let incrementPointer increment state = 
    {state with Pointer = state.Pointer + increment}

let set reg value state = 
    let v = getValue value state.Registers
    state
    |> setValue reg v
    |> incrementPointer 1L

let snd reg state outbox = 
    let v = 
        match reg with
        | FromRegister registerName -> getRegisterValue registerName state.Registers
        | RawValue rawValue -> rawValue
    outbox (Receive v)
    { state with NbSends = state.NbSends + 1L }
    |> incrementPointer 1L

let apply operator reg value state = 
    let oldValue = getRegisterValue reg state.Registers
    let v = getValue value state.Registers
    let newValue = operator oldValue v
    state 
    |> setValue reg newValue
    |> incrementPointer 1L

let add = apply (+)
let mul = apply (*)
let modulo = apply (%)

let rcv registerName state receiver = 
    async {
        let! msg = receiver ()
        let s =
            match msg with
            | Receive value ->
                state
                |> setValue registerName value
                |> incrementPointer 1L
            | _ -> state
        return s
    }


let jgz cond offset state =
    let v = getValue cond state.Registers
    if v > 0L then
        let offsetV = getValue offset state.Registers
        state
        |> incrementPointer offsetV
    else
        state
        |> incrementPointer 1L

let asAsync value = async {return value}

let runInstruction instruction state channel = 
    match instruction with
    | Snd reg -> snd reg state channel.Outgoing |> asAsync
    | Set (reg, value) -> set reg value state |> asAsync
    | Add (reg, value) -> add reg value state |> asAsync
    | Mul (reg, value) -> mul reg value state |> asAsync
    | Mod (reg, value) -> modulo reg value state |> asAsync
    | Rcv reg -> rcv reg state channel.Incoming
    | Jgz (cond, offset) -> jgz cond offset state |> asAsync

let print state =
    let registers =
        state.Registers
        |> Map.toList
        |> List.map (fun (RegisterName r,v) -> sprintf "(%A,%d)" r v)
        |> String.concat ";"
    sprintf 
        "Program: %d, Sends: %d, Pointer: %d, Registers: %s" 
        state.Program
        state.NbSends
        state.Pointer 
        registers

let rec run instructions state channel =
    let l = instructions |> Seq.length |> int64
    if state.Pointer < 0L || state.Pointer >= l then
        state |> asAsync
    else
        let i = instructions |> List.item (int state.Pointer)
        printfn "Executing %A on %s" i (print state)
        async {
            let! newState = runInstruction i state channel
            return! run instructions newState channel
        }

let program initialState instructions =
    MailboxProcessor.Start(fun inbox -> 
        let rec loop state otherProgram = 
            async {
                let! msg = inbox.Receive()

                let other =
                    match msg with
                    | TalkTo otherProgram -> Some otherProgram
                    | _ -> otherProgram
                
                //side effects
                match msg with
                | TalkTo _ -> ()
                | Start -> ()
                | GetState channel -> channel.Reply state
            
                //State update
                let! newState = 
                    match msg with
                    | Start -> 
                        let chan = 
                            { 
                                Incoming = inbox.Receive
                                Outgoing = 
                                    match other with 
                                    | None -> (fun _ -> ())
                                    | Some program -> program.Post
                            }
                        run instructions state chan
                    | GetState _ -> state |> asAsync
                    | TalkTo _ -> state |> asAsync
                
                return! loop newState other
            }

        loop initialState None)

let input = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\input.txt" )
let instructions = input |> parse

let zero = program (init 0L) instructions 
let one = program (init 1L) instructions
zero.Post (TalkTo one)
one.Post (TalkTo zero)

one.Post Start
zero.Post Start
one.PostAndReply GetState
//Executing Rcv (RegisterName 'b') on Program: 1, Sends: 7112, Pointer: 21, Registers: ('a',95);('b',95);('f',0);('i',0);('p',-15)