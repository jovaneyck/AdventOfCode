#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Row = int array
type Layer = Row array
type Image = { dimensions : int*int; layers : Layer list }
type Color = Black | White

let dimensions = (25,6)
let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")

let toLayers (w,h) digits =
    let rows = digits |> Array.chunkBySize w
    let layers = rows |> Array.chunkBySize h |> Array.toList
    layers

let parse (w,h) input : Image =
    let digits = input |> Seq.map (System.Char.GetNumericValue >> int) |> Seq.toArray
    let layers = toLayers (w,h) digits
    { dimensions = (w,h);  layers = layers }

let rec renderPixel (layers : Layer list) (r,c) = 
    let top::ls = layers
    let pixel = top.[r].[c]
    match pixel with
    | 0 -> Black
    | 1 -> White
    | 2 -> renderPixel ls (r,c)
    

let colors (image : Image) : (Color array) array = 
    let (w,h) = image.dimensions
    let pixels =
        seq {
            for r in [0..h-1] do
                for c in [0..w-1] -> (r,c)
        } |> Seq.toList
    pixels
    |> List.map (renderPixel image.layers)
    |> List.toArray
    |> (toLayers image.dimensions)
    |> List.head

let print colors = 
    seq {
        for r in colors do
            for c in r do
                yield 
                    match c with
                    | Black -> ' '
                    | White -> 'O'
            yield '\n'
    }
    |> Seq.map string 
    |> String.concat ""

printf "Testing..."

test <@ parse (3,2) "123456789012" = { dimensions = (3,2); layers = [[|[|1; 2; 3|]; [|4; 5; 6|]|]; [|[|7; 8; 9|]; [|0; 1; 2|]|]] } @>
test <@ parse (2,2) "0222112222120000" |> colors = [|[|Black; White|]; [|White; Black|]|] @>
test <@ print [|[|Black; White|]; [|White; Black|]|] = " O\nO \n" @>
test <@ parse (2,2) "0222112222120000" |> colors |> print = " O\nO \n" @>

printfn "..done"

input
|> parse dimensions
|> colors
|> print
|> printfn "%s"