type Item = 
    | Generator of int //string took longer to compute + more memory space :)
    | Microchip of int
type State = { Elevator : int; Floors : Item list list }

let solved state = 
    state.Elevator = 4
    && 
        state.Floors 
        |> List.except [state.Floors |> List.last] 
        |> (List.forall List.isEmpty)

let between min max element = min <= element && element <= max

let possibleNewFloors state =
    [-1;1] 
    |> List.map (fun offset -> state.Elevator + offset) 
    |> List.filter (between 1 4)

let rec powerset list = 
  match list with
  | [] -> [[]]
  | h :: t -> (powerset t) |> List.collect (fun subset -> [subset; h::subset]) 

let withLengthBetween min max = (List.length >> (between min max))

let isSafeIn items item =
    match item with
    | Generator _ -> true 
    | Microchip m -> 
        (items |> List.contains (Generator m))
        || not <| (items |> List.exists (function | Generator x when x <> m -> true | _ -> false))

let microchipsAreSafe items = 
    items
    |> List.forall (isSafeIn items)

type ItemsToGoAndItemsThatStay = (Item list * Item list) list //Every element is a set of items safe to take and the items remaining

let safeCombinationsToRemove items : ItemsToGoAndItemsThatStay = 
    let candidates =
        items
        |> powerset
        |> List.filter (withLengthBetween 1 2)

    candidates
    |> List.filter microchipsAreSafe
    |> List.map (fun itemsToTake -> 
        let remainingOnFloor = items |> List.except itemsToTake
        (itemsToTake, remainingOnFloor))
    |> List.filter (fun (_, remainingItems) -> remainingItems |> microchipsAreSafe)

let possibleItemsToTakeFromCurrentFloorTo state newFloor : ItemsToGoAndItemsThatStay = 
    let currentFloor = state.Elevator
    let itemsOnCurrentFloor = state.Floors.[currentFloor - 1]
    let itemsOnNewFloor = state.Floors.[newFloor - 1]
    let safeToRemove = itemsOnCurrentFloor |> safeCombinationsToRemove
    let safeToAddToNewFloor = (fun items -> items |> fst |> List.append itemsOnNewFloor |> microchipsAreSafe)
    safeToRemove
    |> List.filter safeToAddToNewFloor

let updateState state newFloor (itemsThatGo, itemsThatStay) =
    let updatedFloors =
        state.Floors
        |> List.mapi 
            (fun floorNb itemsOnFloor ->
                if floorNb = (newFloor - 1) then
                    (itemsThatGo @ itemsOnFloor) |> List.sort //sort @ "change" of items on a floor for lower-cost pruning later
                elif floorNb = (state.Elevator - 1) then
                    itemsThatStay
                else
                    itemsOnFloor)
    {state with Elevator = newFloor; Floors = updatedFloors}

let nextStates state (newFloor, itemsThatGoAndItemsThatStay : ItemsToGoAndItemsThatStay) = 
    itemsThatGoAndItemsThatStay
    |> List.map (updateState state newFloor)

let possibleNextSteps state =
    let newFloors = possibleNewFloors state
    let validMoves = 
        newFloors
        |> List.map (fun newFloor -> (newFloor, possibleItemsToTakeFromCurrentFloorTo state newFloor))
    let nextStates =
        validMoves
        |> List.collect (nextStates state)
    nextStates

let sortAndPrune visitedStates states =
    states
    //|> List.map (fun state -> {state with Floors = state.Floors |> List.map (List.sort)}) //Sort when you add instead of every.single.time.
    |> List.distinct
    |> List.filter (fun state -> visitedStates |> List.contains state |> not) //Not that performant :)

let run start =
    let rec runStep nb states visitedStates =
        printfn "Step %d" nb
        if states |> List.exists solved then
            (nb, states)
        else
            let nextStates = 
                states 
                |> List.collect possibleNextSteps
                |> sortAndPrune visitedStates
                
            runStep (nb + 1) nextStates (nextStates @ visitedStates)

    runStep 0 [start] []

let elements = 
    [
        "H"; "L";"thulium"; "plutonium"; "strontium"; "promethium"; "ruthenium"; "elerium"; "dilithium"
    ]
    |> Seq.indexed
    |> Seq.map (fun (fst, snd) -> (snd, fst))
    |> Map.ofSeq
let lookup el = elements |> Map.find el
let lu = lookup

let example = { Elevator = 1; Floors = [[Microchip (lu "H"); Microchip (lu "L")]; [Generator (lu "H")]; [Generator (lu "L")]; []] }
let p1 = 
    {   Elevator = 1; 
        Floors = 
                [ [Generator (lu "thulium"); Microchip (lu "thulium"); Generator (lu "plutonium"); Generator (lu "strontium")]
                  [Microchip (lu "plutonium"); Microchip (lu "strontium")]
                  [Generator (lu "promethium"); Microchip (lu "promethium"); Generator (lu "ruthenium"); Microchip (lu "ruthenium") ]
                  []]}

let p2 = 
    {   Elevator = 1; 
        Floors = 
                [ [Generator (lu "thulium"); Microchip (lu "thulium"); Generator (lu "plutonium"); Generator (lu "strontium"); Generator (lu "elerium"); Microchip (lu "elerium"); Generator (lu "dilithium"); Microchip (lu "dilithium")]
                  [Microchip (lu "plutonium"); Microchip (lu "strontium")]
                  [Generator (lu "promethium"); Microchip (lu "promethium"); Generator (lu "ruthenium"); Microchip (lu "ruthenium") ]
                  []]}

let (nb, states) = run p1