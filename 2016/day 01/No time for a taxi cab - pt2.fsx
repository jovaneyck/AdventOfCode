let instructions =
    "L4, L1, R4, R1, R1, L3, R5, L5, L2, L3, R2, R1, L4, R5, R4, L2, R1, R3, L5, R1, L3, L2, R5, L4, L5, R1, R2, L1, R5, L3, R2, R2, L1, R5, R2, L1, L1, R2, L1, R1, L2, L2, R4, R3, R2, L3, L188, L3, R2, R54, R1, R1, L2, L4, L3, L2, R3, L1, L1, R3, R5, L1, R5, L1, L1, R2, R4, R4, L5, L4, L1, R2, R4, R5, L2, L3, R5, L5, R1, R5, L2, R4, L2, L1, R4, R3, R4, L4, R3, L4, R78, R2, L3, R188, R2, R3, L2, R2, R3, R1, R5, R1, L1, L1, R4, R2, R1, R5, L1, R4, L4, R2, R5, L2, L5, R4, L3, L2, R1, R1, L5, L4, R1, L5, L1, L5, L1, L4, L3, L5, R4, R5, R2, L5, R5, R5, R4, R2, L1, L2, R3, R5, R5, R5, L2, L1, R4, R3, R1, L4, L2, L3, R2, L3, L5, L2, L2, L1, L2, R5, L2, L2, L3, L1, R1, L4, R2, L4, R3, R5, R3, R4, R1, R5, L3, L5, L5, L3, L2, L1, R3, L4, R3, R2, L1, R3, R1, L2, R4, L3, L3, L3, L1, L2"
    
open System

type Direction =
    | North
    | East
    | South
    | West

type Turn =
    | L
    | R

type Distance = int

type Move = {turn : Turn ; distance : Distance}

type Location = {direction : Direction; location : (int * int)}

let split (separator : string) (line : string) =
    line.Split([|separator|], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray

let splitFirstCharacter (text : string) = 
    (text.Substring(0, 1), text.Substring(1))

let abs (number : int) = 
    Math.Abs(number)

let parseDistance d =
    Int32.Parse(d)

let parseTurn direction =
    match direction with
    | "L" -> L
    | "R" -> R
    | unknown -> failwithf "Unknown direction: %s" unknown

let parseMove (turn, distance) =
    let d = parseDistance distance
    let t = parseTurn turn
    {turn = t; distance = d}

let parse input =
    input
    |> split ", " 
    |> List.map (splitFirstCharacter >> parseMove)

let rotate start direction =
    match start, direction with
    | North, R -> East
    | East, R -> South
    | South, R -> West
    | West, R -> North
    | North, L -> West
    | West, L -> South
    | South, L -> East
    | East, L -> North

let step direction distance (startX, startY) =
    match direction with
    | North -> (startX, startY + distance)
    | East -> (startX + distance, startY)
    | South -> (startX, startY - distance)
    | West -> (startX - distance, startY)


let rec allIntermediarySteps startCoordinates direction distance =
    if distance = 0
    then []
    else
        let nextCoordinate = step direction 1 startCoordinates
        nextCoordinate :: (allIntermediarySteps nextCoordinate direction (distance - 1))

let playMove (location, visitedCoordinates) nextMove = 
    let (currentDirection, currentCoordinates) = location
    let newDirection = rotate currentDirection nextMove.turn
    let newlyVisitedLocations = allIntermediarySteps currentCoordinates newDirection nextMove.distance
    let newCoordinates = newlyVisitedLocations |> List.last
    let newLocation = (newDirection, newCoordinates)
    (newLocation, visitedCoordinates @ newlyVisitedLocations)

let playStartingFrom startingLocation moves = 
    let (_, startingCoordinates) = startingLocation
    moves
    |> List.fold playMove (startingLocation, [startingCoordinates])

let rec firstRevisitedLocation locations =
    match locations with
    | [] -> None
    | loc :: rest -> 
        if rest |> List.contains loc 
        then Some loc
        else firstRevisitedLocation rest

let distanceFrom one other =
    let (x1,y1) = one
    let (x2,y2) = other
    
    abs (abs (x1 - x2)) + (abs (y1 - y2))

let solve problem =
    let startingCoordinates = (0,0)
    let start = (North, startingCoordinates)
    let (_, visitedLocations) =
        problem
        |> parse
        |> playStartingFrom start
    
    let firstRevisited =
        visitedLocations
        |> firstRevisitedLocation

    firstRevisited 
    |> Option.map (distanceFrom startingCoordinates)

let solution = solve instructions