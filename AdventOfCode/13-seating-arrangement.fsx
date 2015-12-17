let input = @"Alice would lose 2 happiness units by sitting next to Bob.
Alice would lose 62 happiness units by sitting next to Carol.
Alice would gain 65 happiness units by sitting next to David.
Alice would gain 21 happiness units by sitting next to Eric.
Alice would lose 81 happiness units by sitting next to Frank.
Alice would lose 4 happiness units by sitting next to George.
Alice would lose 80 happiness units by sitting next to Mallory.
Bob would gain 93 happiness units by sitting next to Alice.
Bob would gain 19 happiness units by sitting next to Carol.
Bob would gain 5 happiness units by sitting next to David.
Bob would gain 49 happiness units by sitting next to Eric.
Bob would gain 68 happiness units by sitting next to Frank.
Bob would gain 23 happiness units by sitting next to George.
Bob would gain 29 happiness units by sitting next to Mallory.
Carol would lose 54 happiness units by sitting next to Alice.
Carol would lose 70 happiness units by sitting next to Bob.
Carol would lose 37 happiness units by sitting next to David.
Carol would lose 46 happiness units by sitting next to Eric.
Carol would gain 33 happiness units by sitting next to Frank.
Carol would lose 35 happiness units by sitting next to George.
Carol would gain 10 happiness units by sitting next to Mallory.
David would gain 43 happiness units by sitting next to Alice.
David would lose 96 happiness units by sitting next to Bob.
David would lose 53 happiness units by sitting next to Carol.
David would lose 30 happiness units by sitting next to Eric.
David would lose 12 happiness units by sitting next to Frank.
David would gain 75 happiness units by sitting next to George.
David would lose 20 happiness units by sitting next to Mallory.
Eric would gain 8 happiness units by sitting next to Alice.
Eric would lose 89 happiness units by sitting next to Bob.
Eric would lose 69 happiness units by sitting next to Carol.
Eric would lose 34 happiness units by sitting next to David.
Eric would gain 95 happiness units by sitting next to Frank.
Eric would gain 34 happiness units by sitting next to George.
Eric would lose 99 happiness units by sitting next to Mallory.
Frank would lose 97 happiness units by sitting next to Alice.
Frank would gain 6 happiness units by sitting next to Bob.
Frank would lose 9 happiness units by sitting next to Carol.
Frank would gain 56 happiness units by sitting next to David.
Frank would lose 17 happiness units by sitting next to Eric.
Frank would gain 18 happiness units by sitting next to George.
Frank would lose 56 happiness units by sitting next to Mallory.
George would gain 45 happiness units by sitting next to Alice.
George would gain 76 happiness units by sitting next to Bob.
George would gain 63 happiness units by sitting next to Carol.
George would gain 54 happiness units by sitting next to David.
George would gain 54 happiness units by sitting next to Eric.
George would gain 30 happiness units by sitting next to Frank.
George would gain 7 happiness units by sitting next to Mallory.
Mallory would gain 31 happiness units by sitting next to Alice.
Mallory would lose 32 happiness units by sitting next to Bob.
Mallory would gain 95 happiness units by sitting next to Carol.
Mallory would gain 91 happiness units by sitting next to David.
Mallory would lose 66 happiness units by sitting next to Eric.
Mallory would lose 75 happiness units by sitting next to Frank.
Mallory would lose 99 happiness units by sitting next to George."

open System

type HappinessChange =
    | Gain of int
    | Loss of int

type Name = | Name of string

type HappinessRelation = {From : Name; To : Name; Change : HappinessChange}
type GuestList = HappinessRelation list

let parseHappiness amount direction = 
    match direction with
    | "gain" -> Gain amount
    | "lose" -> Loss amount
    | unknown -> failwith "Unknown direction: %s" unknown

let interpretTokens = function
    | x :: "would" :: direction :: amount :: "happiness" :: "units" :: "by" :: "sitting" :: "next" :: "to" :: y :: [] -> 
        { 
            From = Name x; 
            Change = (parseHappiness (System.Int32.Parse(amount)) direction); 
            To = Name y
        }
    | unknown -> failwith "Don't know how to interpret %A" unknown

let parseLine (line : string) =
    line.TrimEnd([|'.'|]).Split(' ')
    |> List.ofArray
    |> interpretTokens

let parse (text : string) =
    text.Split('\n')
    |> List.ofArray
    |> List.map parseLine

let rec shuffleIn element xs =
    match xs with
    | [] -> [[element]]
    | h :: t -> 
        let recResult = 
            shuffleIn element t
            |> List.map (fun shuffled -> h :: shuffled)
        (element :: h :: t) :: recResult  

let rec allPermutations l = 
    match l with
    | [] -> [[]]
    | h :: t -> 
        allPermutations t
        |> List.collect (fun recperm -> shuffleIn h recperm)

let allSeatingArrangements guests : Name list list =
    guests
    |> allPermutations
    
let parsedInput = input |> parse
let allGuests = 
    parsedInput
    |> List.collect (fun {From = f; Change = _; To = t} -> [f;t])
    |> List.distinct

let rec lookupHappiness (relations : HappinessRelation list) (Name a) (Name b) =
    match relations with
    | {From = Name x; To = Name y; Change = c} :: _  
        when x = a && y = b-> c
    | {From = Name x; To = Name y; Change = c} :: _  
        when x = "Me" || y = "Me" -> Gain 0
    | _ :: t -> lookupHappiness t (Name a) (Name b)
    | _ -> failwith (sprintf "No happiness relation found between %s and %s" a b)
let happinessFor a b = lookupHappiness parsedInput a b
    
let sumHappiness changes = 
    let rec sumOf c acc =
        match c with
        | [] -> acc
        | Gain g :: t -> sumOf t (acc + g)
        | Loss l :: t -> sumOf t (acc - l)
    sumOf changes 0

let happinessOfArrangement =
    function
    | [] -> 0
    | [el] -> failwith "was not expecting a single visitor, sorry!"
    | h :: t -> 
        h :: t @ [h] |> List.pairwise |> List.collect (fun (x,y) -> [happinessFor x y; happinessFor y x]) |> sumHappiness

let result =
    allGuests 
    |> allSeatingArrangements
    |> List.map (fun arrangement -> (arrangement, happinessOfArrangement arrangement))
    |> List.maxBy (fun (a, h) -> h)