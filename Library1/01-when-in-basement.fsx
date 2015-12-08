let input = "()())"

let rec levels input currentLevel acc = 
    match input with
    | [] -> acc
    | '(' :: t -> (levels t (currentLevel + 1) ((currentLevel + 1) :: acc))
    | ')' :: t -> (levels t (currentLevel - 1) ((currentLevel - 1) :: acc))
    | _ -> failwith "Unrecognized input value"

let whenInBasement input = 
    (levels input 0 []) |> List.rev

let inputAsArray = 
    input.ToCharArray() 
    |> List.ofArray

let allLevels = 
    inputAsArray
    |> whenInBasement 

let levelsWithIndex =
    allLevels
    |> List.zip [1..inputAsArray.Length]

let onlyBasements = 
    levelsWithIndex
    |> List.filter (fun (pos, lvl) -> lvl = -1)