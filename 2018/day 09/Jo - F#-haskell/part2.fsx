//The list-based solution did NOT scale for part 2, going with a data structure that works better with "rotations": enter Deque.
#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
#r @"..\..\dependencies\Jo\.paket\packages\FSharpx.Collections\lib\net40\FSharpx.Collections.dll"
open FSharpx.Collections
open Swensen.Unquote

module Deque = //y u no standard Deque functionality?
    let rec rotate delta dq = 
        let rotateRight delta dq =
            let (x, xs) = dq |> Deque.uncons
            rotate (delta - 1) (xs |> Deque.conj x)
        let rotateLeft delta dq =
            let (xs, x) = dq |> Deque.unconj
            rotate (delta + 1) (xs |> Deque.cons x)

        if delta = 0 then dq
        elif delta > 0 then rotateRight delta dq
        else rotateLeft delta dq

type State = {  circle : Deque<int>
                remainingMarbles : int list
                scoredMarbles : Map<int, uint64 list> //keep track of all taken marbles for all players for debuggability
                nextPlayer : int
                numberPlayers : int }
let startState nbPlayers highestMarble = 
    {circle = Deque.singleton 0
     remainingMarbles = [1..highestMarble]
     scoredMarbles = [1..nbPlayers] |> List.map (fun p -> p,[]) |> Map.ofList
     nextPlayer = 1
     numberPlayers = nbPlayers}

let takeTurn state = 
    let (m::marbles) = state.remainingMarbles
    //printfn "%d" m
    let nextPlayer = (state.nextPlayer % state.numberPlayers) + 1
    if (m % 23 = 0) then
        let leftSeven = state.circle |> Deque.rotate (-7)
        let (taken, newCircle) = leftSeven |> Deque.uncons
        let turnScore = [m; taken] |> List.map uint64
        let oldScore = state.scoredMarbles |> Map.find state.nextPlayer
        let newPoints = state.scoredMarbles |> Map.add state.nextPlayer (turnScore @ oldScore)

        { state with 
            circle = newCircle
            remainingMarbles = marbles
            nextPlayer = nextPlayer
            scoredMarbles = newPoints }
    else
        let twoRight = state.circle |> Deque.rotate 2
        let newCircle = Deque.cons m twoRight
    
        { state with 
            circle = newCircle
            remainingMarbles = marbles
            nextPlayer = nextPlayer }

let score state =
    state.scoredMarbles
    |> Map.map (fun _ marbles -> marbles |> Seq.sum)
    |> Map.toList
    |> List.sortByDescending snd

let rec solve state =
    match state.remainingMarbles with
    | [] -> score state |> List.head |> snd
    | _ -> solve <| takeTurn state

let rec apply times f x =
    if times = 0 then x
    else f (apply (times-1) f x)

let example = startState 9 25
//let played = apply 23 takeTurn example
//played.circle |> Deque.toSeq |> Seq.toList
let dq = Deque.ofList [1..5]

printf "Testing..."
test <@  dq |> Deque.rotate 0 |> Deque.toSeq |> Seq.toList = [1..5] @>
test <@  dq |> Deque.rotate 2 |> Deque.toSeq |> Seq.toList = [3;4;5;1;2] @>
test <@  dq |> Deque.rotate -2 |> Deque.toSeq |> Seq.toList = [4;5;1;2;3] @>
//Acceptance tests
test <@ solve example = 32UL @>
test <@ solve <| startState 10 1618 = 8317UL @>
test <@ solve <| startState 13 7999 = 146373UL @>
test <@ solve <| startState 17 1104 = 2764UL @>
test <@ solve <| startState 21 6111 = 54718UL @>
test <@ solve <| startState 30 5807 = 37305UL @>
printfn "..done!"

//#time 
//before Deque: Real: 00:00:45.426
//after Deque: Real: 00:00:00.049
solve <| startState 416 71975 //439341

//Real: 00:00:07.952 BOOM.
solve <| startState 416 7_197_500 //3566801385