#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let example = @"initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #"

type PotState = Plant | NoPlant

let splitLines (text : string) = text.Split([|"\n"; "\r"|], System.StringSplitOptions.RemoveEmptyEntries) |> Seq.toList |> List.map (fun s -> s.Trim())
let parsePotState = function | '#' -> Plant | '.' -> NoPlant | illegal -> failwithf "illegal pot state: %c" illegal
let parseInitialState (line : string) = line.Substring(15).Trim() |> Seq.map parsePotState |> Seq.toList
let parseRule (line : string) = 
    let a::b::c::d::e::_::_::_::_::next::[] = line.Trim() |> Seq.toList
    let left = [a;b;c;d;e] |> List.map parsePotState
    let right = parsePotState next
    (left, right)

let parse lines =
    let initialState = lines |> List.head |> parseInitialState
    let rules = lines |> List.tail |> List.map parseRule |> Map.ofList
    (initialState |> List.indexed, rules)

let print (state : (int * PotState) list) = 
    let startIndex = state |> List.head |> fst
    let pots = 
        state 
        |> List.map snd 
        |> List.map (function | Plant -> "#" | NoPlant -> ".")
        |> String.concat ""

    printfn "%d: %A" startIndex pots

let apply (rules : Map<PotState list, PotState>) (pots : (int*PotState) list) =
    let configuration = pots |> List.map snd
    let index = pots.[2] |> fst

    rules 
    |> Map.tryFind configuration
    |> (fun o -> defaultArg o NoPlant) //no rule? DIE (for example)
    |> (fun next -> index, next)

let pad state =
    let padding =(List.replicate 5 NoPlant) 
    let last = state |> List.last |> fst
    let frontIndex = state |> List.head |> fst
    let back = padding |> List.zip [last + 1..(last + 5)]
    let front = padding |> List.zip [(frontIndex-5)..(frontIndex-1)]
    let padded = front @ state @ back
    padded

let trim state =
    let trimmedFront = state |> List.skipWhile (snd >> (=) NoPlant)
    let trimmed = trimmedFront |> List.rev |> List.skipWhile (snd >> (=) NoPlant) |> List.rev

    trimmed

let justTick rules = List.windowed 5 >> List.map (apply rules)

let tick rules (state : (int * PotState) list) =
    state
    |> pad
    |> justTick rules
    |> trim

let score state =
    state
    |> List.filter (fun (_,pot) -> pot = Plant)
    |> List.sumBy fst

let rec loop times f x y =
    printfn "%d: %d" times (y |> score)
    if times = 0UL
    then y
    else loop (times - 1UL) f x (f x y)

let solve generations input =
    let (initialState, rules) =
        input
        |> splitLines
        |> parse

    loop generations tick rules initialState
    |> score

printf "Testing..."
test <@ pad [(0, Plant);(1, Plant)] = 
            [(-5, NoPlant); (-4, NoPlant); (-3, NoPlant); (-2, NoPlant); (-1, NoPlant);
            (0, Plant); (1, Plant);
            (2, NoPlant); (3, NoPlant); (4, NoPlant); (5, NoPlant);(6, NoPlant)] @>
test <@ trim [(-1, NoPlant); (0, NoPlant);(1,Plant);(2,NoPlant);(3,Plant);(4,NoPlant)] = [(1, Plant); (2, NoPlant); (3, Plant)] @>

//Acceptance tests
test <@ solve 20UL example = 325 @>

printfn "..done!"

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let part1 = solve 20UL input //2040

//let part2 = solve 50000000000UL input
(*
yeaaaaaa no, that's not doable in finite time.
After a while there's a constant delta between generations: 34

49999999800: 6811
49999999799: 6845
49999999798: 6879
49999999797: 6913
49999999796: 6947
49999999795: 6981
*)
let part2 = 6811UL + (49999999800UL * 34UL)
printfn "%d" part2