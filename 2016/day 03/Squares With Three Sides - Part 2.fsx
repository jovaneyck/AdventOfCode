let columnSeparator = " "
let input = "101 301 501
102 302 502
103 303 503
201 401 601
202 402 602
203 403 603"

let splitLines (lines : string) = 
    lines.Split([|'\n'|])
    |> List.ofSeq

let split (separator : string) (line : string) = 
    line.Split([|separator|], System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofSeq

let parseLength l = 
    System.Int32.Parse(l)

let parseLengths listOfLengths =
    List.map parseLength listOfLengths

let toTuple listOfLengths = 
    match listOfLengths with
    | x :: y :: z :: [] -> (x,y,z)
    | _ -> failwithf "Expecting three lengths of a triangle, but got different amount of lengths: %A" listOfLengths

let rec transpose matrix = 
  match matrix with
  | row :: rows ->
    match row with
    | col :: cols ->
      let first = matrix |> List.map List.head
      let rest = transpose (matrix |> List.map List.tail) 
      first :: rest
    | _ -> []
  | _ -> []

let parse problem =
    problem
    |> splitLines
    |> List.map (split columnSeparator >> parseLengths)
    |> transpose
    |> List.collect (List.chunkBySize 3)
    |> List.map toTuple

let isPossibleTriangle (x,y,z) = 
    x + y > z && x + z > y && y + z > x

input
|> parse
|> List.filter isPossibleTriangle
|> List.length