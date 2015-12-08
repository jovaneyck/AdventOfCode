let input = "^>v<"
let parsed_input = [for c in input -> c]

let rec houses_visited (x,y) acc input =
    match input with
        | [] -> acc
        | h :: t ->
            let next_location = 
                match h with
                | '>' ->  (x + 1, y)
                | '^' -> (x, y + 1)
                | 'v' -> (x, y - 1)
                | '<' -> (x - 1, y)
                | other -> failwith (sprintf "unrecognized input: %c" other)
            houses_visited next_location (next_location :: acc) t

let houses_visited_for input = houses_visited (0,0) [(0,0)] input
let nb_houses_visited input =
    input
    |> houses_visited_for
    |> List.distinct
    |> List.length

//let result = houses_visited_for parsed_input

let nb_houses_visited_robo (input : char list) =
    let (santa_instructions, robo_instructions) = 
        [1..input.Length]
        |> List.zip input
        |> List.partition (fun (instruction, index) -> index % 2 = 0)
    let santa_houses = houses_visited_for (santa_instructions |> List.map (fun (instr, index) -> instr))
    let robo_houses = houses_visited_for (robo_instructions |> List.map (fun (instr, index) -> instr))
    santa_houses @ robo_houses 
    |> List.distinct
    |> List.length

let robo_result = nb_houses_visited_robo parsed_input