type Tile = T | S

let parseChar =
    function
    | '.' -> S
    | '^' -> T
    | unknown -> failwithf "Unknown character in input: %c" unknown

let parse row =
    row
    |> List.ofSeq
    |> List.map parseChar

let stateOfTileBasedOn =
    function
    | (T, T, S)
    | (S, T, T)
    | (T, S, S)
    | (S, S, T) -> T
    | _         -> S

let orDefault d v = defaultArg v d

let getStateAtIndex row index = 
    row 
    |> List.tryItem index
    |> orDefault S

let nextRow currentRow =
    let getState = getStateAtIndex currentRow
    currentRow
    |> List.mapi (fun i _ -> (getState (i - 1), getState i, getState (i + 1)))
    |> List.map stateOfTileBasedOn

let generate firstRow nbRows =
    List.unfold 
        (fun grid -> 
            let rowNb = (grid |> List.length)
            if rowNb > nbRows
            then
                None 
            else 
                printfn "Generating row %d" rowNb
                let lastRow = grid.[0]
                let next = nextRow lastRow
                Some (lastRow, next :: grid))
        [parse firstRow]

let numberOf state grid =
    grid
    |> List.collect id
    |> List.filter ((=) state)
    |> List.length

#r @"..\..\0 lib\F#\Unquote.dll"
open Swensen.Unquote

test <@ parse "..^^." = [S;S;T;T;S] @>
test <@ stateOfTileBasedOn (T,S,S) = T @>
test <@ stateOfTileBasedOn (S,S,S) = S @>
test <@ parse ".^^^^" = (nextRow <| parse "..^^.") @>
test <@ parse "^^..^" = (nextRow <| parse ".^^^^") @>

test <@ generate "..^^." 3 = [[S; S; T; T; S]; [S; T; T; T; T]; [T; T; S; S; T]] @>
test <@ (numberOf S (generate ".^^.^.^^^^" 10)) = 38 @>

let input = ".^^^^^.^^.^^^.^...^..^^.^.^..^^^^^^^^^^..^...^^.^..^^^^..^^^^...^.^.^^^^^^^^....^..^^^^^^.^^^.^^^.^^"
#time
let pt1 = numberOf S <| generate input 40

//Doesn't keep the entire grid in memory
// just the last row + # of safe tiles in every row.
let fasterCount firstRow nbRows =
    List.unfold 
        (fun (rowNb, lastRow) -> 
            if rowNb > nbRows
            then
                None 
            else 
//                printfn "Generating row %d" rowNb
                let next = nextRow lastRow
                Some (numberOf S [lastRow], (rowNb + 1, next)))
        (1, parse firstRow)
    |> List.sum
let pt1fast = fasterCount input 40
#time
let pt2 = fasterCount input 400000