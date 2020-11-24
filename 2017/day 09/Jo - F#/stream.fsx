#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

//Complete rewrite after watching Michael Gililand solve this with a nice parser approach [here](https://www.youtube.com/watch?v=7VrgBixxzBs)

type Parser<'a> = char list -> 'a*char list
type Group = Group of Group list
type Garbage = unit

let map f parser = parser >> (fun (parsed, rest) -> (f parsed), rest)
let cons x xs = x :: xs

let rec parseGroupOrGarbage: Choice<Group, Garbage> Parser = 
    fun txt ->
        match txt with
        | '{'::inner -> map Choice1Of2 parseGroup txt
        | '<'::inner -> map Choice2Of2 parseGarbage txt
        | _ -> failwith "Expecting a group or garbage"
and parseGroup: Group Parser =
  let rec parseInner = function
    | '}'::rest -> ([], rest)
    | ','::text
    | text ->
        match parseGroupOrGarbage text with
        | (Choice1Of2 group, rest) -> map (cons group) parseInner rest
        | (Choice2Of2 _, rest)-> parseInner rest
  function
  | '{'::inner -> map Group parseInner inner
  | _ -> failwith "Expecting to parse a group"
and parseGarbage: Garbage Parser =
  let rec parseInner = 
    function
    | '!' :: _ :: rest -> parseInner rest
    | '>' :: rest -> ((), rest)
    | _ :: rest -> parseInner rest
    | [] -> failwith "Garbage was not closed correctly"

  function
  | '<'::inner -> parseInner inner
  | _ -> failwith "Expecting to parse garbage"

let score group =
  let rec scoreByLevel n (Group group) =
    n + (group |> Seq.sumBy (scoreByLevel (n + 1)))

  scoreByLevel 1 group

let assertParse text =
    parseGroup (text |> Seq.toList) |> fst
printf "Testing..."
test <@ assertParse "{}" = Group [] @>
test <@ assertParse "{{{}}}" = Group [Group [Group []]] @>
test <@ assertParse "{{},{}}" = Group [Group [] ;Group []] @>
printfn "..done!"

let input = System.IO.File.ReadAllText <| __SOURCE_DIRECTORY__ + "\input.txt" |> List.ofSeq

//part 1
input
|> parseGroup
|> fst
|> score 
|> printfn "%d"