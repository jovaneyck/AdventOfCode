open System

type Location = string
type Distance = int
type TravelDistance = {From : Location; To : Location; Distance : Distance}
type Route = Location list
type RouteDistance = {Route : Route; Distance : Distance}

let textInput = "Tristram to AlphaCentauri = 34
Tristram to Snowdin = 100
Tristram to Tambi = 63
Tristram to Faerun = 108
Tristram to Norrath = 111
Tristram to Straylight = 89
Tristram to Arbre = 132
AlphaCentauri to Snowdin = 4
AlphaCentauri to Tambi = 79
AlphaCentauri to Faerun = 44
AlphaCentauri to Norrath = 147
AlphaCentauri to Straylight = 133
AlphaCentauri to Arbre = 74
Snowdin to Tambi = 105
Snowdin to Faerun = 95
Snowdin to Norrath = 48
Snowdin to Straylight = 88
Snowdin to Arbre = 7
Tambi to Faerun = 68
Tambi to Norrath = 134
Tambi to Straylight = 107
Tambi to Arbre = 40
Faerun to Norrath = 11
Faerun to Straylight = 66
Faerun to Arbre = 144
Norrath to Straylight = 115
Norrath to Arbre = 135
Straylight to Arbre = 127"

let parse = function
    | f :: "to" :: t :: "=" :: d :: [] -> {From = f; To = t; Distance = Int32.Parse(d)}
    | unknown -> failwith "Could not parse distance %S" unknown

let parsedInput : TravelDistance list =
    textInput.Split('\n')
    |> List.ofArray
    |> List.map (fun l -> l.Split(' ') |> List.ofArray)
    |> List.map parse

let input = parsedInput
//    [
//        {From = "London"; To = "Dublin"; Distance = 464};
//        {From = "London"; To = "Belfast"; Distance = 518};
//        {From = "Dublin"; To = "Belfast"; Distance = 141}
//    ]

let calculateDistance distances fromLoc toLoc =
    distances
    |> List.find(fun {From = f; To = t; Distance = d} -> 
        (f = fromLoc && t = toLoc) || (f = toLoc && t = fromLoc))
    |> (fun dist -> dist.Distance)

let getAllLocations travelDistances = 
    travelDistances
    |> List.collect (fun {From = f; To = t; Distance = _} -> [f;t])
    |> List.distinct

let allLocations = getAllLocations input

let rec shuffleIn (element : string) (xs : string list) : (string list list) =
    match xs with
    | [] -> [[element]]
    | h :: t -> 
        let recResult = 
            shuffleIn element t
            |> List.map (fun shuffled -> h :: shuffled)
        (element :: h :: t) :: recResult  

let rec allPermutations (l : Location list) : (Route list) = 
    match l with
    | [] -> [[]]
    | h :: t -> 
        allPermutations t
        |> List.collect (fun recperm -> shuffleIn h recperm)

let distanceOfRoute (cities : Route) (distances : TravelDistance list) : Distance = 
    cities
    |> List.pairwise
    |> List.map(fun (fst, snd) -> calculateDistance input fst snd)
    |> List.sum

let allRoutesWithDistance = 
    allLocations
    |> allPermutations
    |> List.map(fun r -> (r, (distanceOfRoute r input)))

let shortestRoute =
    allRoutesWithDistance
    |> List.minBy (fun (_, d) -> d)

let longestRoute =
    allRoutesWithDistance
    |> List.maxBy (fun (_, d) -> d)