let parse (input : string) =
    input.Split(' ')
    |> Seq.map System.Int32.Parse
    |> List.ofSeq

type Tree = { Children : Tree list; Metadata : int list }

let rec buildNodes (nbNodes : int) (data : int list) : (Tree list * int list) =
    if nbNodes = 0 then ([],data)
    else
        let (nbChildren :: nbMetadata :: tail) = data
        let (children, restAfterChildren) = buildNodes nbChildren tail
        let (metadata, restAfterMetadata) = List.splitAt nbMetadata restAfterChildren
        let (nextNodes, finalTail) = buildNodes (nbNodes - 1) restAfterMetadata

        ( { Children = children; Metadata = metadata } :: nextNodes, finalTail)

let rec value node =
    if node.Children |> Seq.length = 0 then
        node.Metadata |> Seq.sum
    else
        let indexes = node.Metadata
        indexes
        |> Seq.choose (fun i -> node.Children |> Seq.tryItem (i - 1))
        |> Seq.map value
        |> Seq.sum

let solve input = 
    let ([root], []) =
        input
        |> parse
        |> buildNodes 1
    root
    |> value

#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
printf "Testing..."

test <@ solve "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" = 66 @>

printfn "..done!"

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
solve input