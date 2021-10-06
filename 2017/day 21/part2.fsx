#r "nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

type State = On | Off
type Grid = State[,]
type Recipe = Grid * Grid
type Recipes = Map<Grid,Grid>

let parseState = function | '#' -> On | '.' -> Off | u -> failwith $"Unknown state: {u}"

let parseGrid (text : string) : Grid =
    let lines = text.Split('/')
    [| for line in lines do [| for c in line -> parseState c|]|]
    |> array2D 

let parseRecipe (text : string) : Recipe = 
    let [|src;dest|] = text.Split([|" => "|],System.StringSplitOptions.None)
    (parseGrid src, parseGrid dest)

let parseRecipes (lines: string seq) : Recipe seq =
    lines
    |> Seq.map parseRecipe

let chunkBy (size : int) (m : 'a[,]) : 'a[,][,] = 
    let chunks = (m |> Array2D.length1) / size - 1
    let indices = [for i in 0..chunks -> i * size]
    [ for i in indices do 
        [for j in indices -> 
            m.[i..i+(size-1),j..j+(size-1)]]]
    |> array2D

let join (m : 'a[,][,]) : 'a[,] = 
    let outerDim = Array2D.length1 m
    let innerDim = Array2D.length1 m.[0,0]
    [for outer in 0..outerDim - 1 do
        for inner in 0..innerDim - 1 ->
            m.[outer,*] |> Seq.collect (fun m -> m.[inner,*])]
    |> array2D

let rotate (m : 'a[,]) = 
    let lastColumn = (m |> Array2D.length1) - 1
    [for c in [lastColumn .. -1 .. 0] -> m.[*,c]]
    |> array2D

let flip (m : 'a[,]) = [for r in [(Array2D.length1 m - 1) .. (-1) .. 0] -> m.[r,*]] |> array2D

let orientations (m : 'a[,]) : 'a[,] seq =     
    seq { 
        yield m
        let r1 = rotate m
        yield r1
        let r2 = rotate r1
        yield r2
        let r3 = rotate r2
        yield r3
        yield flip m
        yield flip r1
        yield flip r2
        yield flip r3
    }
    |> Seq.distinct

let recipeOrientations recipe =
    let src, dest = recipe
    src |> orientations |> Seq.map (fun s-> s,dest)

let chunk (g: Grid) : Grid[,] =
    let size = Array2D.length1 g
    if size % 2 = 0 
    then chunkBy 2 g 
    else chunkBy 3 g

let buildRecipes : string seq -> Recipes = parseRecipes >> Seq.collect recipeOrientations >> Map.ofSeq

///Find and apply the first recipe that matches
let apply (recipes : Recipes) (g : Grid) = 
    g 
    |> orientations 
    |> Seq.pick (fun o -> recipes |> Map.tryFind o)

let next (recipes : Recipes) (g : Grid) : Grid =
    g
    |> chunk
    |> Array2D.map (apply recipes)
    |> join

let rec repeat n f x =
    printfn "."
    if n = 0 then x
    else 
        let next = f x
        repeat (n - 1) f next

let part2 input start =
    let recipes = input |> buildRecipes
    let parsed = start |> parseGrid
    repeat 18 (next recipes) parsed

let start = ".#./..#/###"
part2 input start |> Seq.cast<State> |> Seq.filter ((=)On) |> Seq.length

printf "Testing..."
test <@ parseRecipe ".##/###/##. => .#.#/.##./.##./##.." = (array2D [ [Off;On;On];[On;On;On];[On;On;Off] ], array2D [ [Off;On;Off;On];[Off;On;On;Off];[Off;On;On;Off];[On;On;Off;Off] ]) @>
test <@ array2D [ [Off;On];[On;Off] ] |> chunk =  array2D [[array2D [[Off; On]; [On; Off]]]] @>
test <@ array2D [ [Off;On;Off;On];[Off;On;On;Off];[Off;On;On;Off];[On;On;Off;Off] ] |> chunk = array2D [[array2D [[Off; On]; [Off; On]]; array2D [[Off; On]; [On; Off]]];[array2D [[Off; On]; [On; On]]; array2D [[On; Off]; [Off; Off]]]] @>
printfn "..done!"

