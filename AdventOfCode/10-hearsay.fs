module HearSay

open System

let rec hearSayChars acc chars = 
    match chars with
    | [] -> acc
    | x :: xs -> 
        hearSayChars 
            ((x :: (xs |> List.takeWhile ((=) x)))::acc)
            (xs |> List.skipWhile ((=) x))

let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None ->
                let res = f x
                cache := (!cache).Add(x,res)
                res

let hearSay (number : string) =
    number.ToCharArray()
    |> List.ofArray
    |> ((memoize hearSayChars) [])
    |> List.rev
    |> List.map (fun identicals -> (identicals |> List.length, identicals |> List.head))
    |> List.map (fun (nb, char) -> nb.ToString() + char.ToString())
    |> List.reduce (+)

let rec repeat f times input =
    printfn "Iteration %d - input has length %d" times (input |> String.length)
    match times with
    | 0 -> input
    | n -> repeat f (n - 1) (f input)

let repeatHearSay = repeat (memoize hearSay)