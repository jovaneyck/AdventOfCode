open System

let rec hearSayChars chars = 
    match chars with
    | [] -> []
    | x :: xs -> 
        let equalToX = (x :: (xs |> List.takeWhile ((=) x)))
        let tail = (xs |> List.skipWhile ((=) x) |> hearSayChars)
        equalToX :: tail

let hearSay (number : string) =
    number.ToCharArray()
    |> List.ofArray
    |> hearSayChars
    |> List.map (fun identicals -> (identicals |> List.length, identicals |> List.head))
    |> List.map (fun (nb, char) -> nb.ToString() + char.ToString())
    |> List.reduce (+)

let rec repeat f times input =
    match times with
    | 0 -> input
    | n -> repeat f (n - 1) (f input)

let repeatHearSay = repeat hearSay 40

let result = repeatHearSay "1113122113"