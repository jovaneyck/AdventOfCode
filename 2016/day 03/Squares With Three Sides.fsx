let input = "775  785  361
  622  375  125
  297  839  375
  245   38  891
  503  463  849
  731  482  759"

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

let parse problem =
    problem
    |> splitLines
    |> List.map (split "  ")
    |> List.map parseLengths
    |> List.map toTuple

let isPossibleTriangle (x,y,z) = 
    x + y > z && x + z > y && y + z > x

input
|> parse
|> List.filter isPossibleTriangle
|> List.length