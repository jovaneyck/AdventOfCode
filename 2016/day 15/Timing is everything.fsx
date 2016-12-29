open System

type Disc = { id : int; numberOfPositions : int; position : int }
type state = { time : int; level : int; discs : Disc list }

let rotateDisc delta disk = 
    let newPosition = (disk.position + delta) % disk.numberOfPositions
    {disk with position = newPosition}

let rotateDiscs delta state = 
    { state with discs = state.discs |> List.map (rotateDisc delta) }

let offsetTime delta state =
    {state with time = state.time + delta}

let offsetLevel delta state =
    {state with level = state.level + delta}

let tick time state = 
    state
    |> offsetTime time
    |> rotateDiscs time

let moveDown state =
    state
    |> tick 1
    |> offsetLevel 1

let fallsThroughDisk d = d.position = 0

//Yuck. Clean up this procedural muck a bit
let rec fallsThrough state = 
    if state.level = 0 then
        fallsThrough (state |> moveDown)
    else
        let diskAtCurrentLevel = state.discs.[state.level - 1]
        if not <| fallsThroughDisk diskAtCurrentLevel then
            false
        else
            let atEnd = state.level = state.discs.Length
            if atEnd then true 
            else fallsThrough (state |> moveDown)

let debug extractor stream =
    stream
    |> Seq.map (fun x -> printfn "%A" <| extractor x; x)

let firstTimeToFallThrough input =
    Seq.initInfinite id
    |> Seq.map (fun offset -> (offset, tick offset input))
//    |> debug (fun (offset,_) -> offset)
    |> Seq.find (fun (t, s) -> fallsThrough s)

let example =
    [
        {id = 1; numberOfPositions = 5; position = 4}
        {id = 2; numberOfPositions = 2; position = 1}
    ]

let pt1 =
    [
        {id = 1; numberOfPositions = 13; position = 1}  
        {id = 2; numberOfPositions = 19; position = 10}  
        {id = 3; numberOfPositions = 3; position = 2}  
        {id = 4; numberOfPositions = 7; position = 1}  
        {id = 5; numberOfPositions = 5; position = 3}  
        {id = 6; numberOfPositions = 17; position = 5}  
    ]

let pt2 =
    [
        {id = 1; numberOfPositions = 13; position = 1}  
        {id = 2; numberOfPositions = 19; position = 10}  
        {id = 3; numberOfPositions = 3; position = 2}  
        {id = 4; numberOfPositions = 7; position = 1}  
        {id = 5; numberOfPositions = 5; position = 3}  
        {id = 6; numberOfPositions = 17; position = 5}  
        {id = 7; numberOfPositions = 11; position = 0}  
    ]

let result = firstTimeToFallThrough {time = 0; level = 0; discs = example}
//let result = firstTimeToFallThrough {time = 0; level = 0; discs = pt1}
//let result = firstTimeToFallThrough {time = 0; level = 0; discs = pt2}
printfn "Found it: %A" (fst result)