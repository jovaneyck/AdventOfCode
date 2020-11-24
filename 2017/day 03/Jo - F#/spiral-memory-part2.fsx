// Strategy: 
//* generate memory locations one-by-one in an infinite stream
//* store calculated (location, value) in a map
//* Calculate the next value by looking up all the neighbours in the map
//* Until stopping condition hit

type Step = R | U | L | D

let move location step = 
    let (x,y) = location
    match step with
    | R -> (x + 1, y)
    | U -> (x, y + 1)
    | D -> (x, y - 1)
    | L -> (x - 1, y)

let steps location number = 
    let (x,y) = location
    let moves = 
        if number % 2 = 1 then
            //<number> to the right, <number> to top
            List.append (List.replicate number R) (List.replicate number U)
        else
            //<number to the left, <number> down
            List.append (List.replicate number L) (List.replicate number D)
    moves
    |> Seq.scan move location

let memoryLocations = 
    let scanner locations number =
        let lastLoc = locations |> Seq.last
        steps lastLoc number

    seq {
        let start = (0,0)
        yield start
        yield!
            Seq.initInfinite id
            |> Seq.scan scanner (Seq.singleton start)
            |> Seq.collect id
    }

let neighbours location =
    let (x,y) = location
    let deltas = [(-1,1); (0,1); (1,1); (-1,0); (1,0); (-1, -1); (0, -1); (1,-1)]
    
    deltas
    |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))

let calculate location currentValues = 
    let neighbouringValues =
        location
        |> neighbours
        |> Seq.choose (fun loc -> currentValues |> Map.tryFind loc)
    if neighbouringValues |> Seq.isEmpty then 1 //starting value
    else neighbouringValues |> Seq.sum

let runUntil threshold =
    memoryLocations
    |> Seq.scan
        (fun values loc -> 
            let calculated = calculate loc values
            values |> Map.add loc calculated)
        Map.empty
    |> Seq.map (fun m -> 
        let calculatedValues = m |> Map.toSeq |> Seq.map snd 
        if calculatedValues |> Seq.isEmpty then 0
        else calculatedValues |> Seq.max)
    |> Seq.find (fun max -> max > threshold)

runUntil 289326