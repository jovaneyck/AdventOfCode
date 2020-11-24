type Position = {x : int; y : int}

let gridLocationsOffset offset length = 
    [for x in offset.x..(offset.x + length) do
        for y in offset.y..(offset.y + length) ->
            {x = x; y = y}]

let gridLocations gridSize =
    [for x in 1..gridSize do
        for y in 1..gridSize -> 
            {x = x; y = y} ]

let topLefts gridSize squareSize =
    gridLocations (gridSize - squareSize)

let hundreds nb = (nb / 100) % 10
let powerLevel serial {x = x; y = y} = 
    let rackID = x + 10
    (hundreds ((rackID * y + serial) * rackID)) - 5

let totalPowerLevel squareSize powerLevels topLeft =
    gridLocationsOffset topLeft (squareSize - 1)
    |> List.map (fun loc -> powerLevels |> Map.find loc)
    |> List.sum

let solve gridSize squareSize serialNumber =
    let powerLevels =
        gridLocations gridSize
        |> List.map (fun pos -> pos, powerLevel serialNumber pos)
        |> Map.ofList
    topLefts gridSize squareSize
    |> List.map (fun tl -> tl, totalPowerLevel squareSize powerLevels tl)
    |> List.sortByDescending snd
    |> List.head

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

printf "Testing..."
test <@ hundreds 12345 = 3 @>
test <@ hundreds 99 = 0 @>
test <@ powerLevel 8 {x=3;y=5} = 4 @>
test <@ powerLevel 57 {x=122;y=79} = -5 @>
test <@ powerLevel 39 {x=217;y=196} = 0 @>
test <@ powerLevel 71 {x=101;y=153} = 4 @>
test <@ gridLocations 2 = [{x = 1;y = 1;}; {x = 1;y = 2;}; {x = 2;y = 1;}; {x = 2;y = 2;}] @>
test <@ topLefts 2 1 = [{x = 1;y = 1;}] @>

test <@ solve 300 3 18 = ({x = 33; y = 45}, 29) @>
test <@ solve 300 3 42 = ({x = 21; y = 61}, 30) @>
printfn "..done!"

//What is the X,Y coordinate of the top-left fuel cell of the 3x3 square with the largest total power?
//Your puzzle input is 7400.
solve 300 3 7400