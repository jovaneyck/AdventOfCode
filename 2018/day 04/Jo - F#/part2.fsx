open System
open System.Text.RegularExpressions

type Event = BeginsShift of int | FallsAsleep | WakesUp 
type Log = { timeStamp : DateTime; event : Event }
type Action = { actor : int; log : Log }

let parseDate date =
    match DateTime.TryParse(date) with
    | (true, d) -> d
    | failed -> failwithf "Failed to parse date %A" failed

let parseEvent =
    function
    | "falls asleep" -> FallsAsleep
    | "wakes up" -> WakesUp
    | beginsShift -> 
        let parseInt = Int32.Parse
        let pattern = @"Guard #(\d*) begins shift"
        let m = Regex.Match(beginsShift, pattern)
        let get (i : int) = m.Groups.[i].Value
        get 1 |> parseInt |> BeginsShift
            
let parse line =
    let pattern = @"\[(.*)\] (.*)"
    let m = Regex.Match(line, pattern)
    let get (i : int) = m.Groups.[i].Value

    { timeStamp = parseDate <| get 1
      event = parseEvent <| get 2 }

let toActions (currentId, acc) (log : Log) = 
    let nextId =
        match log.event with
        | BeginsShift id -> id
        | _ -> currentId
    let action = { actor = nextId; log = log }
    (nextId, action :: acc)

type State = { lastSleepStart : int option; sleeps : (int * int) list }
let toSleepTime actions = 
    let sleep state action = 
        match action.log.event with
        | BeginsShift _ -> state
        | FallsAsleep -> { state with lastSleepStart = Some action.log.timeStamp.Minute }
        | WakesUp -> 
            let (Some start) = state.lastSleepStart
            { state with lastSleepStart = None; sleeps = (start, action.log.timeStamp.Minute) :: state.sleeps }
        
    List.fold sleep {lastSleepStart = None; sleeps = []} actions

let findMostSleptMinute sleeps : (int * int) option =
    let toSleepingMinutes (start, stop) = [start..stop-1]
    
    sleeps
    |> List.collect toSleepingMinutes
    |> List.groupBy id
    |> List.sortByDescending (snd >> List.length)
    |> List.tryHead
    |> Option.map (fun (minute, occurences) -> (minute, occurences |> List.length))

let solve input =
    let actionsPerGuard =
        input
        |> List.map parse
        |> List.sortBy (fun event -> event.timeStamp)
        |> List.fold toActions (-1, []) |> (fun (_, l) -> List.rev l)
        |> List.groupBy (fun a -> a.actor)
    let sleepTimes = 
        actionsPerGuard
        |> List.map (fun (id, actions) -> (id, findMostSleptMinute (toSleepTime actions).sleeps))
    sleepTimes
    |> List.choose (fun (id,sleepTime) -> sleepTime |> Option.map (fun st -> (id, st)))
    |> List.sortByDescending (fun (_, (_,timesAsleep)) -> timesAsleep)
    |> List.map (fun (id, (mostSleptMinute, _)) -> (id, mostSleptMinute, id * mostSleptMinute))
    |> List.head
    

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let example = [
    "[1518-11-01 00:00] Guard #10 begins shift"
    "[1518-11-01 00:05] falls asleep"
    "[1518-11-01 00:25] wakes up"
    "[1518-11-01 00:30] falls asleep"
    "[1518-11-01 00:55] wakes up"
    "[1518-11-01 23:58] Guard #99 begins shift"
    "[1518-11-02 00:40] falls asleep"
    "[1518-11-02 00:50] wakes up"
    "[1518-11-03 00:05] Guard #10 begins shift"
    "[1518-11-03 00:24] falls asleep"
    "[1518-11-03 00:29] wakes up"
    "[1518-11-04 00:02] Guard #99 begins shift"
    "[1518-11-04 00:36] falls asleep"
    "[1518-11-04 00:46] wakes up"
    "[1518-11-05 00:03] Guard #99 begins shift"
    "[1518-11-05 00:45] falls asleep"
    "[1518-11-05 00:55] wakes up"]

printf "Testing..."
test <@ solve example = (99, 45, 4455) @>
printfn "..done!"

let input = 
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "\input.txt") 
    |> Seq.toList

solve input |> (fun (_,_,result) -> result)