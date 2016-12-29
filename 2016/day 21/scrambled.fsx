#r "..\..\0 lib\F#\Unquote.dll"
open Swensen.Unquote
open System
open System.IO
open System.Text.RegularExpressions

type Direction = Left | Right
type Command =
    | SwapPosition of (int * int)
    | SwapLetter of (char * char)
    | Rotate of (Direction * int)
    | RotateBasedOnPositionOf of char
    | ReversePositions of (int * int)
    | MoveToPosition of (int * int)

//Blatant ripoff from burningmonk/Gert's regex unapply pattern match magic
let (|R|_|) pattern input =
    let m = Regex.Match(input, pattern)
    match m.Success with
    | false -> None
    | true -> Some <| List.skip 1 [for group in m.Groups -> group.Value]

let (|Int|_|) input = 
    match Int32.TryParse input with
    | false,_ -> None
    | true, v -> Some v

let (|Char|_|) input = 
    match Char.TryParse input with
    | false,_ -> None
    | true, v -> Some v

let parseCommand text =
    match text with
    | R "swap position (\d) with position (\d)" [Int X;Int Y] -> SwapPosition(X, Y)
    | R "swap letter (\w) with letter (\w)" [Char X; Char Y] -> SwapLetter(X, Y)
    | R "rotate left (\d) step(s?)" [Int X; _] -> Rotate(Left, X)
    | R "rotate right (\d) step(s?)" [Int X; _] -> Rotate(Right, X)
    | R "rotate based on position of letter (\w)" [Char X] -> RotateBasedOnPositionOf(X)
    | R "reverse positions (\d) through (\d)" [Int X; Int Y] -> ReversePositions(X, Y)
    | R "move position (\d) to position (\d)" [Int X; Int Y] -> MoveToPosition(X, Y)
    | unknown -> failwithf "Unknown command: %s" unknown

let parse (commands : string) =
    commands.Split([|'\n'|])
    |> Seq.map parseCommand

let toString = Seq.map string >> String.concat ""

let swapPosition (text : 'a list) (x,y) = 
    let atX = text.[x]
    let atY = text.[y]
    text 
    |> List.mapi 
        (fun idx value -> 
            if idx = x then atY
            elif idx = y then atX
            else value)

let swapLetter text (one,two) =
    text
    |> List.map 
        (function 
        | l when l = one -> two 
        | l when l = two -> one 
        | otherwise -> otherwise)

let rotateLeft text distance = 
    let rec r text distance =
        match distance, text with
        | 0, _ -> text
        | d, h :: t -> r (t @ [h]) (d - 1)
        | illegal -> failwithf "Did not expect to rotate %A" illegal
    r text distance

let rotateRight text distance = 
    rotateLeft (text |> List.rev) distance |> List.rev

let rotateBasedOnPositionOf text letter = 
    let index = text |> List.findIndex ((=) letter)
    let offset = 1 + index + (if index >= 4 then 1 else 0)
    rotateRight text offset

let reversePositions text (x,y) = 
    let (before, remainder) = text |> List.splitAt x
    let (interestingPart, after) = remainder |> List.splitAt (y - x + 1)
    before @ (interestingPart |> List.rev) @ after

let insertAtIndex index letter list = 
    let rec ins index list acc =
        match index, list with
        | 0, l -> (acc |> List.rev) @ [letter] @ l
        | i, h :: t -> ins (i - 1) t (h :: acc)
        | unexpected -> failwithf "Unexpected insert case: %A" unexpected
    ins index list []

let moveToPosition (text : char list) (x,y) = 
    let letter = text.[x]
    text
    |> List.except [letter]
    |> insertAtIndex y letter

let execute text command =
    match command with 
    | SwapPosition p -> swapPosition text p
    | SwapLetter l -> swapLetter text l
    | Rotate(Left, d) -> rotateLeft text d
    | Rotate(Right, d) -> rotateRight text d
    | RotateBasedOnPositionOf l -> rotateBasedOnPositionOf text l
    | ReversePositions p -> reversePositions text p
    | MoveToPosition p -> moveToPosition text p

let run secret commands = 
    commands |> Seq.fold execute (secret |> Seq.toList)

let scramble secret input =
    input
    |> parse
    |> (run secret)
    |> toString

let rec distribute e = 
    function
    | [] -> [[e]]
    | h::t as xs -> (e::xs)::[for xs in distribute e t -> h::xs]

let rec permute = 
    function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

let unscramble (scrambled : string) input =
    let permutations = scrambled |> Seq.toList |> permute
    permutations 
    |> Seq.find (fun possibility -> scrambled = scramble possibility input)
    |> toString

printfn "Testing..."
parseCommand "swap position 3 with position 2" =! SwapPosition(3,2)
parseCommand "swap letter d with letter f" =! SwapLetter('d','f')
parseCommand "rotate left 1 step" =! Rotate(Left,1)
parseCommand "rotate right 1 step" =! Rotate(Right,1)
parseCommand "rotate left 3 steps" =! Rotate(Left,3)
parseCommand "rotate right 2 steps" =! Rotate(Right,2)
parseCommand "rotate based on position of letter f" =! RotateBasedOnPositionOf('f')
parseCommand "reverse positions 0 through 4" =! ReversePositions(0,4)
parseCommand "move position 3 to position 0" =! MoveToPosition(3,0)

let testExecute text cmd = execute (text |> List.ofSeq) cmd |> Seq.map string |> String.concat ""
testExecute "abcde" <| SwapPosition(4,0) =! "ebcda"
testExecute "ebcda" <| SwapLetter('d','b') =! "edcba"
testExecute "edcba" <| ReversePositions(0,4) =! "abcde"
testExecute "abcde" <| ReversePositions(1,3) =! "adcbe"
testExecute "abcde" <| Rotate(Left, 1) =! "bcdea"
testExecute "abcde" <| Rotate(Left, 2) =! "cdeab"
testExecute "abcde" <| Rotate(Right, 2) =! "deabc"
testExecute "abcde" <| Rotate(Right, 3) =! "cdeab"
testExecute "bcdea" <| MoveToPosition(1,4) =! "bdeac"
testExecute "bdeac" <| MoveToPosition(3,0) =! "abdec"
testExecute "abdec" <| RotateBasedOnPositionOf('b') =! "ecabd"
testExecute "ecabd" <| RotateBasedOnPositionOf('d') =! "decab"
testExecute "abcde" <| RotateBasedOnPositionOf('a') =! "eabcd"
testExecute "abcde" <| RotateBasedOnPositionOf('c') =! "cdeab"
testExecute "abcde" <| RotateBasedOnPositionOf('e') =! "eabcd"

let example = @"swap position 4 with position 0
swap letter d with letter b
reverse positions 0 through 4
rotate left 1 step
move position 1 to position 4
move position 3 to position 0
rotate based on position of letter b
rotate based on position of letter d"

scramble "abcde" example =! "decab"

printfn "Done!"

let input = File.ReadAllText(System.IO.Path.Combine([| __SOURCE_DIRECTORY__; "input.txt"|]))
let pt1 = scramble "abcdefgh" input
let pt2 = unscramble "fbgdceah" input