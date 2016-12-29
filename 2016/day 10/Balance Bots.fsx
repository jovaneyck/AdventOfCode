//Disclaimer: this is one big hacked together script. Don't read this if your goal is to learn clean F# code :)

#r "..\..\0 lib\F#\Unquote.dll"
#r "..\..\0 lib\F#\FSharp.Text.RegexProvider.dll"

open System
open System.Text.RegularExpressions
open Swensen.Unquote
open FSharp.Text.RegexProvider

type BotState =  { botId : int; first : int option; second : int option }
type OutputState = {outputId : int; value : int}
type InitialData = {botId : int; value : int}
type Recipient =
    | Bot of int
    | Output of int
type GiveData = {gifter : int; lowRecipient : Recipient; highRecipient : Recipient}
type Instruction = 
    | Initial of InitialData
    | Give of GiveData

let parseInt = Int32.Parse

type GoesToRegex = Regex<"value (?<value>\d+) goes to bot (?<bot>\d+)">
let (|GoesTo|_|) input =
    let regex = new GoesToRegex()
    let match' = regex.Match(input)
    match match'.Success with
    | false -> None
    | true -> Some (match'.value.Value |> parseInt, match'.bot.Value |> parseInt)

type GivesToBotRegex = Regex<"bot (?<gifter>\d+) gives low to (?<lowRecipientType>.*) (?<lowRecipient>\d+) and high to (?<highRecipientType>.*) (?<highRecipient>\d+)">
let buildRecipient type' id =
    match type' with
    | "bot" -> Bot id
    | "output" -> Output id
    | unknown -> failwithf "Unknown recipient type: %s" unknown
let (|GivesToBot|_|) input =
    let regex = new GivesToBotRegex()
    let match' = regex.Match(input)
    match match'.Success with
    | false -> None
    | true ->         
        let lowRecipient = buildRecipient (match'.lowRecipientType.Value) (match'.lowRecipient.Value |> parseInt)
        let highRecipient = buildRecipient (match'.highRecipientType.Value) (match'.highRecipient.Value |> parseInt)
        Some (match'.gifter.Value |> parseInt, lowRecipient, highRecipient)

let parse = 
    function
    | GoesTo (value, bot) -> Initial { botId = bot; value = value }
    | GivesToBot (gifter, lowRecipient, highRecipient) -> Give {gifter = gifter; lowRecipient = lowRecipient; highRecipient = highRecipient}
    | unknown -> failwithf "I encountered an instruction I cannot parse: %s" unknown

let buildBot botId valuesToHold =
    match valuesToHold with
    | x :: y :: [] -> {botId = botId; first = Some x; second = Some y}
    | x :: [] -> {botId = botId; first = Some x; second = None}
    | [] -> {botId = botId; first = None; second = None}
    | _ -> failwithf "A bot is designed to hold two items MAX. Bot <%d> is given <%A>" botId valuesToHold
let buildInitialState instructions =
    let initializeInstructions =
        instructions |> Seq.choose (function | Initial data -> Some data | _ -> None)
    initializeInstructions
    |> Seq.groupBy (fun data -> data.botId)
    |> Seq.map (fun grouping -> buildBot (fst grouping) (snd grouping |> Seq.map (fun data -> data.value) |> Seq.toList))
    |> Seq.toList

let onlyGiveInstructions instructions =
    instructions
    |> Seq.choose (function | Give data -> Some data | _ -> None)

test <@ parse "value 5 goes to bot 2" = Initial {botId = 2; value = 5} @>
test <@ parse "value 52 goes to bot 24" = Initial {botId = 24; value = 52} @>
test <@ parse "bot 2 gives low to bot 1 and high to bot 0" = Give {gifter = 2; lowRecipient = Bot 1; highRecipient = Bot 0} @>
test <@ parse "bot 2 gives low to output 3 and high to output 0" = Give {gifter = 2; lowRecipient = Output 3; highRecipient = Output 0} @>
test <@ buildInitialState [Give {gifter = 2; lowRecipient = Bot 1; highRecipient = Bot 2}; Initial {botId = 2; value = 5}] |> List.ofSeq 
            = [{botId = 2; first = Some 5; second = None}] @>

open System.IO
let input = File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
//let input = "value 5 goes to bot 2
//bot 2 gives low to bot 1 and high to bot 0
//value 3 goes to bot 1
//bot 1 gives low to output 1 and high to bot 0
//bot 0 gives low to output 2 and high to output 0
//value 2 goes to bot 2".Split('\n')

let instructions = Seq.map parse input
let startState = buildInitialState instructions    
let giveInstructions = onlyGiveInstructions instructions |> Seq.toList

let give item recipient  = 
    match recipient with
    | {first = None} -> {recipient with first = Some item}
    | {second = None} -> {recipient with second = Some item}
    | _ -> failwithf "Bot is receiving an item but has his hands full: %A" (recipient, item)
    
let orDefault default' value = defaultArg value default'

let apply (state : BotState list) {gifter = gifterId; lowRecipient = lowRecipient; highRecipient = highRecipient} =
    let gifterState = state |> Seq.find (function | {botId = id} -> id = gifterId)
    let (lowId, highId) =
        match gifterState.first, gifterState.second with
        | None, _
        | _, None ->
            failwithf "A bit more work is required, gift instructions are executed at a point where the bot doesn't have 2 items"
        | Some a, Some b -> if a < b then (a,b) else (b,a)

//    //Reeeaaaaally hacky, but getting tired :)    
//    if lowId = 17 && highId = 61 then
//        failwithf "BOT %d is handling 17 & 61" gifterState.botId


    let lowRecipientID = //Hacky output again. Who says FP has to be pure :P
        match lowRecipient with
        | Output id -> 
            printfn "Something is hitting output: %A" {outputId = id; value = lowId}
            None
        | Bot id -> 
            Some id
    let highRecipientID =
        match highRecipient with
        | Output id -> 
            printfn "Something is hitting output: %A" {outputId = id; value = highId}
            None
        | Bot id -> 
            Some id

    let orDefaultEmptyBotWithId id =
        function 
        | None -> Some {botId = id; first = None; second = None} 
        | some -> some

    let lowRecipientState =
        lowRecipientID |> Option.bind (fun id -> state |> List.tryFind (fun b -> b.botId = id) |> orDefaultEmptyBotWithId id)
    let highRecipientState =
        highRecipientID |> Option.bind (fun id -> state |> List.tryFind (fun b -> b.botId = id) |> orDefaultEmptyBotWithId id)

    let impactedBots = [Some gifterState; lowRecipientState; highRecipientState] |> Seq.choose id
    let notImpactedBots = state |> List.except impactedBots

    let newGifter = {gifterState with first = None; second = None}
    let newLowRecipient = lowRecipientState |> Option.map (give lowId)
    let newHighRecipient = highRecipientState |> Option.map (give highId)
    let updatedImpacted = [Some newGifter; newLowRecipient; newHighRecipient] |> List.choose id
    updatedImpacted @ notImpactedBots

let findNextPossibleInstruction state instructions = 
    let botsWithHandsFull =
        state
        |> List.filter (function | {first = Some _; second = Some _} -> true | _ -> false)
        |> List.map (fun {botId = id} -> id)
    let instructionForBotWithHandsFull =
        instructions
        |> List.tryFind (fun i -> botsWithHandsFull |> List.contains i.gifter)
    match instructionForBotWithHandsFull with
    | None -> failwithf "No possible next instruction. WHAT state: %A remaining instructions: %A" state instructions
    | Some instruction -> (instruction, instructions |> List.except [instruction])

let rec applyInstructions instructions state =
    match instructions with 
    | [] -> state
    | _ -> 
        let (next, remainingInstructions) = findNextPossibleInstruction state instructions
        applyInstructions remainingInstructions (apply state next)

applyInstructions giveInstructions startState