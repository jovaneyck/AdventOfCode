#r "nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"

//Does the problem reduce to "find me the index n given a row and column?"
//Let's solve naively first: just build the grid

let diagonalIndices n =
    let rec diagonalIndices acc x y =
        if y = 0 
        then acc |> List.rev
        else diagonalIndices ((x,y) :: acc) (x+1) (y-1)
    diagonalIndices [] 1 n

let indices = Seq.initInfinite (fun i -> diagonalIndices (i+1)) |> Seq.collect id |> Seq.cache
let index coord = 1 + (indices |> Seq.findIndex ((=) coord))

let seed = 20151125L
let next value =
    value * 252533L % 33554393L

let rec repeat n f x =
    if n = 1
    then x
    else repeat (n-1) f (f x)

let idx = index (3019,3010) //18168397
let part1 = repeat idx next seed

printf "Testing.."
test <@ indices |> Seq.take 21 |> Seq.toList = [(1, 1); (1, 2); (2, 1); (1, 3); (2, 2); (3, 1); (1, 4); (2, 3); (3, 2); (4, 1);(1, 5); (2, 4); (3, 3); (4, 2); (5, 1); (1, 6); (2, 5); (3, 4); (4, 3); (5, 2);(6, 1)] @>
test <@ index (6,1) = 21 @>
test <@ seed |> next = 31916031L @>
test <@ seed |> next |> next = 18749137L @>
test <@ repeat 21 next seed = 33511524L @>
printfn "..done!"