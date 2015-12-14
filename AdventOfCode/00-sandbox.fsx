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
    |> List.map (fun ident -> (ident |> List.length, ident |> List.head))
    |> List.map (fun (cnt, c) -> cnt.ToString() + c.ToString())
    |> List.reduce (+)

let rec repeat f times input =
    match times with
    | 0 -> input
    | n -> repeat f (n - 1) (f input)

let repeatHearSay = repeat hearSay 40

let result = repeatHearSay "1113122113"