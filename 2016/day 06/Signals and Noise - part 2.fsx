let splitLines (text : string) = 
    text.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)

let indexed list =
    list
    |> Seq.indexed

let columns lines =
    lines
    |> Seq.collect indexed
    |> Seq.groupBy (fun (index,_) -> index)
    |> Seq.map (fun (_, grouping) -> grouping |> Seq.map snd)

let elementWithMostOccurences =
    Seq.countBy id 
    >> Seq.sortBy (fun (_,count) -> count) 
    >> Seq.head 
    >> fst 

let decode secret = 
    secret
    |> splitLines
    |> columns
    |> Seq.map  elementWithMostOccurences
    |> Seq.map string
    |> String.concat ""
    

#r @".\lib\Unquote.dll"
open Swensen.Unquote

let example = "eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar"

printfn "testing"
test <@ elementWithMostOccurences ['c';'a';'c'] = 'c' @>
test <@ decode example = "advent" @>
printfn "testing done!"