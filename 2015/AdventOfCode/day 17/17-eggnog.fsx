let rec possibleCombinations amount c = 
    match c with
    | [] -> [[]]
    | h :: t  when h <= amount -> 
        let withTail = possibleCombinations (amount - h) t |> List.map(fun el -> h :: el)
        let withoutTail = possibleCombinations amount t
        withTail @ withoutTail
    | _ :: t -> possibleCombinations amount t

let validCombinations amount containers =
    let isValid combo = 
        combo 
        |> List.sum 
        |> (=) amount
    possibleCombinations amount containers
    |> List.filter isValid

let adventInput = @"50
44
11
49
42
46
18
32
26
40
21
7
18
43
10
47
36
24
22
40"

let parsedInput = adventInput.Split('\n') |> List.ofArray |> List.map(fun el -> el |> System.Int32.Parse)
let exampleContainers = [20;15;10;5;5]

validCombinations 150 parsedInput |> List.groupBy List.length |> List.minBy fst |> snd |> List.length