type State = {  circle : int list
                currentMarbleIndex : int
                remainingMarbles : int list
                scoredMarbles : Map<int, int list> //keep track of all taken marbles for all players
                nextPlayer : int
                numberPlayers : int }
let startState nbPlayers highestMarble = 
    {circle = [0]
     currentMarbleIndex = 0
     remainingMarbles = [1..highestMarble]
     scoredMarbles = [1..nbPlayers] |> List.map (fun p -> p,[]) |> Map.ofList
     nextPlayer = 1
     numberPlayers = nbPlayers}

let wrapIndex max index = 
    if index < 0 then
        (max + (index % max) ) % max
    else
        index % max

let takeTurn state = 
    let wrap = wrapIndex (state.circle |> Seq.length)
    let (m::marbles) = state.remainingMarbles
    printfn "%d" m
    let nextPlayer = (state.nextPlayer % state.numberPlayers) + 1
    if (m % 23 = 0) then
        let marbleToTake = wrap (state.currentMarbleIndex - 7)
        let (before, taken :: after) = state.circle |> List.splitAt marbleToTake
        let newCurrentMarbleIndex = marbleToTake
        let newCircle = before @ after

        let turnScore = [m; taken]
        let currentScore = state.scoredMarbles |> Map.find state.nextPlayer
        let newPoints = state.scoredMarbles |> Map.add state.nextPlayer (turnScore @ currentScore)

        { state with 
            circle = newCircle
            currentMarbleIndex = newCurrentMarbleIndex
            remainingMarbles = marbles
            nextPlayer = nextPlayer
            scoredMarbles = newPoints }
    else
        let oneClockwise = wrap (state.currentMarbleIndex + 1)
        let (before, after) = state.circle |> List.splitAt (oneClockwise + 1)
        let newCurrentMarbleIndex = before |> Seq.length
        let newCircle = List.concat [before; [m]; after]
    
        { state with 
            circle = newCircle
            currentMarbleIndex = newCurrentMarbleIndex
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

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let rec apply times f x =
    if times = 0 then x
    else f (apply (times-1) f x)

let example = startState 9 25
let played = apply 25 takeTurn example
printf "Testing..."

test <@ wrapIndex 3 0 = 0 @>
test <@ wrapIndex 3 2 = 2 @>
test <@ wrapIndex 3 3 = 0 @>
test <@ wrapIndex 3 4 = 1 @>
test <@ wrapIndex 3 -1 = 2 @>
test <@ wrapIndex 3 -2 = 1 @>
test <@ wrapIndex 3 -3 = 0 @>
test <@ played.circle = [0; 16; 8; 17; 4; 18; 19; 2; 24; 20; 25; 10; 21; 5; 22; 11; 1; 12; 6; 13; 3; 14; 7; 15] @>
test <@ played.currentMarbleIndex = 10 @>
test <@ score played = [(5, 32); (1, 0); (2, 0); (3, 0); (4, 0); (6, 0); (7, 0); (8, 0); (9, 0)] @>
//Acceptance tests
test <@ solve example = 32 @>
test <@ solve <| startState 10 1618 = 8317 @>
test <@ solve <| startState 13 7999 = 146373 @>
test <@ solve <| startState 17 1104 = 2764 @>
test <@ solve <| startState 21 6111 = 54718 @>
test <@ solve <| startState 30 5807 = 37305 @>
printfn "..done!"

//416 players; last marble is worth 71975 points
//#time //Real: 00:00:45.426
//solve <| startState 416 71975