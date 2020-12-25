type Status =
    | Running of int
    | Resting of int
type ReindeerSpecification = {Name : string; Speed : int; SprintDuration : int; RestDuration : int}
type State = {Status : Status; Distance : int; Reindeer: ReindeerSpecification}

let stepState (state : State)= 
    match state with
    | {Status = Resting 0; Distance = d; Reindeer = r} -> {Status = Running (r.SprintDuration - 1); Distance = d + r.Speed; Reindeer = r}
    | {Status = Resting toRest; Distance = d; Reindeer = r} -> {Status = Resting (toRest - 1); Distance = d; Reindeer = r}
    | {Status = Running 0; Distance = d; Reindeer = r} -> {Status = Resting (r.RestDuration - 1); Distance = d; Reindeer = r}
    | {Status = Running toGo; Distance = d; Reindeer = r} -> {Status = Running (toGo - 1); Distance = d + r.Speed; Reindeer = r}

let step states = 
    states
    |> List.map stepState

let rec generateSeries (generation, state) = seq {
    yield (generation, state)
    yield! generateSeries (generation + 1, step state)
}

let initialState spec = {Status = Resting 0; Reindeer = spec; Distance = 0}

let timeSeries specs = 
    generateSeries (0, specs |> List.map initialState)
    |> Seq.find (fun (time, _) -> time = 2503)
    |> snd
    |> List.maxBy (fun {Distance = d} -> d)
    |> (fun {Reindeer = {Name = n }; Distance = d} -> (n,d))

let demoSpecs = [
    {Name = "Comet"; Speed = 14; SprintDuration = 10; RestDuration = 127};
    {Name = "Dancer"; Speed = 16; SprintDuration = 11; RestDuration = 162}
]

let rawSpecs = @"Vixen can fly 19 km/s for 7 seconds, but then must rest for 124 seconds.
Rudolph can fly 3 km/s for 15 seconds, but then must rest for 28 seconds.
Donner can fly 19 km/s for 9 seconds, but then must rest for 164 seconds.
Blitzen can fly 19 km/s for 9 seconds, but then must rest for 158 seconds.
Comet can fly 13 km/s for 7 seconds, but then must rest for 82 seconds.
Cupid can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
Dancer can fly 3 km/s for 16 seconds, but then must rest for 37 seconds.
Prancer can fly 25 km/s for 6 seconds, but then must rest for 143 seconds."

let parseNb = System.Int32.Parse
let interpretTokens = function
    | name :: "can" :: "fly" :: speed :: "km/s" :: "for" :: sprintDuration :: "seconds," :: "but" :: "then" :: "must" :: "rest" :: "for" :: restDuration :: "seconds." :: []
        -> {Name = name; Speed = parseNb speed; SprintDuration = parseNb sprintDuration; RestDuration = parseNb restDuration}
    | unknown -> failwith "Could not parse %A" unknown
let parseLine (l : string) =
    l.Split(' ')
    |> List.ofArray
    |> interpretTokens

let parse (specs : string) =
    specs.Split('\n')
    |> List.ofArray
    |> List.map parseLine

rawSpecs
|> parse
|> timeSeries

generateSeries (1, rawSpecs |> parse |> List.map initialState) |> Seq.take 2 |> List.ofSeq