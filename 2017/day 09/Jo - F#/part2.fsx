//Complete rewrite after watching Michael Gililand solve this with a nice parser approach [here](https://www.youtube.com/watch?v=7VrgBixxzBs)

type Parser<'a> = char list -> 'a*char list
type Garbage = char list

let map f parser = parser >> (fun (parsed, rest) -> (f parsed), rest)
let cons x xs = x :: xs

let rec parseGroupOrGarbage: Garbage Parser = 
    fun txt ->
        match txt with
        | '{'::inner -> parseGarbageFromGroup txt
        | '<'::inner -> parseGarbage txt
        | _ -> failwith "Expecting a group or garbage"
and parseGarbageFromGroup: Garbage Parser =
  let rec parseInner = function
    | '}'::rest -> ([], rest)
    | ','::text
    | text ->
        match parseGroupOrGarbage text with
        | (garbage , rest)-> map (List.append garbage) parseInner rest
  function
  | '{'::inner -> parseInner inner
  | _ -> failwith "Expecting to parse a group"
and parseGarbage: Garbage Parser =
  let rec parseInner = 
    function
    | '!' :: _ :: rest -> parseInner rest
    | '>' :: rest -> ([], rest)
    | g :: rest -> map (cons g) parseInner rest
    | [] -> failwith "Garbage was not closed correctly"

  function
  | '<'::inner -> parseInner inner
  | _ -> failwith "Expecting to parse garbage"
  
let input = System.IO.File.ReadAllText <| __SOURCE_DIRECTORY__ + "\input.txt" |> List.ofSeq

//part 2
input
|> parseGarbageFromGroup
|> fst
|> Seq.length
|> printfn "%d"