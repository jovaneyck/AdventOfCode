(*
Naive part 2 (recalculate for every square size) does not scale.
General outline of the "part 2" algorithm:
* data structure: matrix instead of dictionary
* build upon results from previous square size N
* for N+1:  add power levels of the "outer edges"
* if an N+1 square drops out of the grid, ignore it
*)
let serial = 7400
type Position = {x : int; y : int}

let gridLocations gridSize =
   [for x in 1..gridSize do
       for y in 1..gridSize ->
           {x = x; y = y} ]

let hundreds nb = (nb / 100) % 10
let powerLevel serial {x = x; y = y} =
   let rackID = x + 10
   (hundreds ((rackID * y + serial) * rackID)) - 5

let powerLevelMatrix = Array2D.init 300 300 (fun r c -> powerLevel serial {x = r + 1; y = c + 1})

type AggregatePowerLevel = { topLeft : Position; bottomRight : Position; level : int }
type State = { aggregates : AggregatePowerLevel list; optimum : int * AggregatePowerLevel }

let startState =
   let init = gridLocations 300 |> List.map (fun loc -> {topLeft = loc; bottomRight = loc; level = powerLevel serial loc})
   let initialBest = init |> List.maxBy(fun agg -> agg.level) |> fun agg -> (1, agg)
   { aggregates = init; optimum = initialBest }

(*
Growing illustrated: X = region to add. If out of bounds, Nothing of interest.
   x ->
y
|
v
       tl . .  .   X
        . . .  .   X
        . . .  .   X
        . . . br   X
        [X X X X  nbr] => row
*)

let grow aggregate =
   let newBottomRight = { x = aggregate.bottomRight.x + 1; y = aggregate.bottomRight.y + 1 }
   if newBottomRight.x >= 300 || newBottomRight.y >= 300
   then None
   else
       let row =
           [ for x in aggregate.topLeft.x..newBottomRight.x -> { x = x; y = newBottomRight.y } ]
       let column =
           [ for y in aggregate.topLeft.y..(newBottomRight.y - 1) -> { x = newBottomRight.x; y = y }]
       let increase =
           row @ column
           |> List.map (fun loc -> powerLevelMatrix.[loc.x - 1, loc.y - 1])
           |> List.sum
       Some { aggregate with bottomRight = newBottomRight; level = aggregate.level + increase }

let folder state squareSize =
   printfn "%d" squareSize
   let increased = state.aggregates |> List.choose (fun agg -> grow agg)
   let bestForSize = increased |> List.maxBy (fun agg -> agg.level)
   { state with 
        aggregates = increased
        optimum = if bestForSize.level > (snd state.optimum).level then (squareSize, bestForSize) else state.optimum }

#time //Real: 00:01:29.221, CPU: 00:01:29.484, GC gen0: 36631, gen1: 957, gen2: 44
let result =
   [2..299]
   |> List.fold folder startState
let (size, { topLeft = {x = x; y = y} }) = result.optimum
printfn "Solution: %d,%d,%d" x y size //233,187,13