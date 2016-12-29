open System
open System.Security.Cryptography

type Direction = U | D | L | R

let hasher = MD5.Create()
let hash (word : string) =
    let bytes = System.Text.Encoding.ASCII.GetBytes(word)
    let hash = hasher.ComputeHash(bytes)
    BitConverter.ToString(hash).Replace("-", "").ToLower()

let isOpen =
    function
    | 'b'
    | 'c'
    | 'd'
    | 'e'
    | 'f' -> true
    |  _  -> false

let openDoors hash =
    match hash |> List.ofSeq with
    | u :: d :: l :: r :: _ -> 
        [(U,isOpen u);(D, isOpen d);(L, isOpen l); (R, isOpen r)]
        |> List.filter snd
        |> List.map fst
    | _ -> failwith "Hashes should contain at least 4 characters"


let inGrid (x,y) =
    1 <= x && x <= 4 && 1 <= y && y <= 4

let move (x,y) direction =
    match direction with
    | U -> (x, y-1)
    | D -> (x, y+1)
    | L -> (x - 1, y)
    | R -> (x + 1, y)

let doorString = function | U -> "U" | D -> "D" | L -> "L" | R -> "R"
        
let nextSteps secret (location, path) =
    let h = hash (secret + path)
    let openDirections = openDoors h
    openDirections
    |> List.map (fun d -> (move location d, path + (doorString d)))
    |> List.filter (fun (l,p) -> inGrid l)

let reachedDestination destination (location, _) = 
    location = destination

let rec shortestPath destination currentPaths secret =
    match currentPaths |> List.tryFind (reachedDestination destination) with
    | Some (_,p) -> p
    | None ->
        let next =
            currentPaths
            |> List.collect (fun p -> nextSteps secret p)
        shortestPath destination next secret

let solvept1 = shortestPath (4,4) [((1,1), "")]

let orDefault d v = defaultArg v d

let longestLength paths = 
    paths
    |> List.map snd
    |> List.map String.length
    |> List.sortDescending
    |> List.tryHead
    |> orDefault 0

let rec maxLength currentMaxLength destination currentPaths secret =
    let (ended, stillgoing) = 
        currentPaths 
        |> List.partition (reachedDestination destination) 
    let newLongest = max currentMaxLength (longestLength ended)
    if stillgoing |> List.isEmpty then newLongest
    else
        let next = stillgoing |> List.collect (fun p -> nextSteps secret p)
        maxLength newLongest destination next secret

let solvept2 secret = maxLength 0 (4,4) [((1,1), "")] secret
    
#r @"..\..\0 lib\F#\Unquote.dll"
open Swensen.Unquote

test <@ ['b'..'f'] |> List.forall isOpen @>
test <@ ['a'] @ ([0..9] |> List.map (string >> Seq.head)) |> List.forall (isOpen >> not) @>

test <@ (hash "hijkl").StartsWith("ced9") @>

test <@ openDoors "ced9" = [U;D;L] @>

test <@ solvept1 "ihgpwlah" = "DDRRRD" @>
test <@ solvept1 "kglvqrro" = "DDUDRLRRUDRD" @>
test <@ solvept1 "ulqzkmiv" = "DRURDRUDDLLDLUURRDULRLDUUDDDRR" @>

solvept1 "gdjjyniy"

test <@ solvept2 "ihgpwlah" = 370 @>
test <@ solvept2 "kglvqrro" = 492 @>
test <@ solvept2 "ulqzkmiv" = 830 @>

#time
solvept2 "gdjjyniy"