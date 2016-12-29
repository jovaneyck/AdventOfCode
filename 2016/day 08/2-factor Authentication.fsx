#r ".\lib\Unquote.dll"
open Swensen.Unquote

open System
open System.IO
open System.Text.RegularExpressions

let readLines path =
    File.ReadLines(path)
    
type PixelState = Off | On
type Screen = {width : int; height : int; locations : ((int * int) * PixelState) list}
type RectDetails = {width : int; height : int}
type Section = Row of int | Column of int
type RotateDetails = { section : Section; offset : int }
type Command = 
    | Rect of RectDetails
    | Rotate of RotateDetails

let parseInt text =
    Int32.Parse text

let valueOf (matcher : Match) (group : string) = 
    matcher.Groups.[group].Value

let intValueOf matcher group = 
    valueOf matcher group |> parseInt

let (|IsRect|_|) line =
    let matcher = Regex.Match(line, "rect (?<width>\d*)x(?<height>\d*)")
    if matcher.Success then
        Some <| IsRect {width = intValueOf matcher "width"; height = intValueOf matcher "height"}
    else None

let (|IsRotateRow|_|) line =
    let matcher = Regex.Match(line, "rotate row y=(?<row>\d*) by (?<offset>\d*)")
    if matcher.Success then
        Some <| IsRotateRow {section = Row <| intValueOf matcher "row"; offset = intValueOf matcher "offset"}
    else None

let (|IsRotateColumn|_|) line =
    let matcher = Regex.Match(line, "rotate column x=(?<column>\d*) by (?<offset>\d*)")
    if matcher.Success then
        Some <| IsRotateColumn {section = Column <| intValueOf matcher "column"; offset = intValueOf matcher "offset"}
    else None

let parse (line : string) =
    match line with
    | IsRect dimensions -> Rect dimensions
    | IsRotateRow details -> Rotate details
    | IsRotateColumn details -> Rotate details
    | _ -> failwithf "Unknown instruction: %s" line

let allCoordinatesBetween (xmin, ymin) (xmax, ymax) =
    [for r in ymin..ymax do
     for c in xmin..xmax -> (c,r)]

let emptyScreenOfDimensions (width, height) = 
    let coordinates = 
        allCoordinatesBetween (0,0) (width-1, height-1)
    let allOff = 
        coordinates 
        |> List.map (fun coord -> (coord, Off))
    {locations = allOff; width = width; height = height}

let applyRect screen ({width = w; height = h} : RectDetails) =
    let coordinatesToTurnOn = allCoordinatesBetween (0,0) (w-1, h-1)
    let newLocations =
        screen.locations 
        |> List.map 
            (fun (coordinate, originalState) -> 
                if coordinatesToTurnOn |> List.contains coordinate 
                then (coordinate, On)
                else (coordinate, originalState))
    { screen with locations = newLocations }

let rotate width height section offset (x,y) = 
    match section with
    | Row r when y = r -> 
        ((x + offset) % width, y)
    | Column c when x = c -> 
        (x, (y + offset) % height)
    | _ -> 
        (x,y)

let applyRotate (screen : Screen) { section = section; offset = offset} = 
    let newLocations = 
        screen.locations
        |> List.map (fun (coord, state) -> 
            (rotate screen.width screen.height section offset coord, state))
    { screen with locations = newLocations }

let apply screen command =
    match command with
    | Rect details -> applyRect screen details
    | Rotate details -> applyRotate screen details

let nbOfOnPixels screen =
    screen.locations
    |> Seq.filter (fun (_,state) -> state = On)
    |> Seq.length

let run input seed =
    input
    |> Seq.map parse
    |> Seq.fold apply seed

printfn "Testing..."
test <@ (run ["rect 3x2"] (emptyScreenOfDimensions (7,3))).locations
            =
                [((0, 0), On); ((1, 0), On); ((2, 0), On); ((3, 0), Off); ((4, 0), Off);((5, 0), Off); ((6, 0), Off); 
                 ((0, 1), On); ((1, 1), On); ((2, 1), On); ((3, 1), Off); ((4, 1), Off); ((5, 1), Off); ((6, 1), Off); 
                 ((0, 2), Off); ((1, 2), Off); ((2, 2), Off); ((3, 2), Off); ((4, 2), Off); ((5, 2), Off); ((6, 2), Off)] @>
test <@ (run ["rect 3x2"; "rotate column x=1 by 1"] (emptyScreenOfDimensions (7,3))).locations
            =
                [((0, 0), On); ((1, 1), On); ((2, 0), On); ((3, 0), Off); ((4, 0), Off); ((5, 0), Off); ((6, 0), Off); 
                 ((0, 1), On); ((1, 2), On); ((2, 1), On); ((3, 1), Off); ((4, 1), Off); ((5, 1), Off); ((6, 1), Off); 
                 ((0, 2), Off);((1, 0), Off); ((2, 2), Off); ((3, 2), Off); ((4, 2), Off); ((5, 2), Off);((6, 2), Off)] @>
printfn "Testing done!"

let input = 
    readLines <| Path.Combine [|__SOURCE_DIRECTORY__; "input.txt"|]

let endScreen =
    run input (emptyScreenOfDimensions (50,6))
nbOfOnPixels endScreen