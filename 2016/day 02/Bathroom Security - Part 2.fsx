let input = "ULL
RRDDD
LURDL
UUUUD"

type Direction = U | D | L | R
let startingButton = '5'
let validMoves = 
    [
        ('1', D), '3'
        ('2', R), '3'
        ('2', D), '6'
        ('3', U), '1'
        ('3', L), '2'
        ('3', D), '7'
        ('3', R), '4'
        ('4', L), '3'
        ('4', D), '8'
        ('5', R), '6'
        ('6', L), '5'
        ('6', U), '2'
        ('6', R), '7'
        ('6', D), 'A'
        ('7', L), '6'
        ('7', U), '3'
        ('7', R), '8'
        ('7', D), 'B'
        ('8', L), '7'
        ('8', U), '4'
        ('8', R), '9'
        ('8', D), 'C'
        ('9', L), '8'
        ('A', U), '6'
        ('A', R), 'B'
        ('B', L), 'A'
        ('B', U), '7'
        ('B', R), 'C'
        ('B', D), 'D'
        ('C', L), 'B'
        ('C', U), '8'
        ('D', U), 'B'
    ]
    |> Map.ofList

open System

let splitLines (text : string) = 
    text.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray

let parseDirection d = 
    match d with
    | 'U' -> U
    | 'D' -> D
    | 'L' -> L
    | 'R' -> R
    | _ -> failwithf "Unknown direction: %c" d

let parseLine line =
    line
    |> List.ofSeq
    |> List.map parseDirection

let parse input =
    input
    |> splitLines
    |> List.map parseLine

let orIfNotFound def value = defaultArg value def

let takeStep location direction = 
    validMoves 
    |> Map.tryFind (location, direction)
    |> orIfNotFound location

let findNextButton (start, codeUntilNow) steps =
    let nextButton = steps |> List.fold takeStep start
    (nextButton, codeUntilNow @ [nextButton])

let solve problem =
    problem
    |> parse
    |> List.fold findNextButton (startingButton, [])

solve input