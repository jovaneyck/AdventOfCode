type Password = string
type PasswordCharacter = char
type OverflowOccured = bool

let incrementChar el : PasswordCharacter * OverflowOccured = 
    let maxValue = (int)'z'
    let nextValue = ((int)el + 1)
    match nextValue > maxValue with
    |true   -> ('a',true) 
    | _     -> ((char)nextValue, false)

let rec increment chars = 
    match chars with
    | [] -> []
    | h :: t -> 
        match incrementChar h with
        | (next, true) -> next :: (increment t)
        | (next, _) -> next :: t

let next (password : Password) = 
    password.ToCharArray()
    |> List.ofArray
    |> List.rev
    |> increment
    |> List.rev
    |> System.String.Concat
    
let rec straight (pw : char list) = 
    let rawIncrement el = (char)((int)el + 1)
    pw
    |> (fun chars -> 
            match chars with
            | [] -> false
            | [_] -> false
            | [_;_] -> false
            | a :: b :: c :: t when
                b = (rawIncrement a) && c = (rawIncrement b) -> true
            | h :: t -> straight t)

let pipe f =
    function 
    | None -> None
    | Some x -> 
        match f x with
        | true -> Some x
        | false -> None

let rec noIllegal (chars : char list) =
    match chars with
    | [] -> true
    | h :: t when ['i';'o';'l'] |> List.contains h -> false
    | _ :: t -> noIllegal t

let rec pairs nb chars =
    if nb = 0 then true
    else 
        match chars with
        | [] -> false
        | [_] -> false
        | a :: b :: t when a = b -> pairs (nb - 1) t
        | h :: t -> pairs nb t

let twoPairs chars = pairs 2 chars

let validPassword (password : Password) = 
    Some (password.ToCharArray() |> List.ofArray)
    |> (pipe straight)
    |> (pipe noIllegal)
    |> (pipe twoPairs)
    |> (fun res -> 
            match res with
            | Some pw -> true
            | None -> false) 

let passwordCandidates seed =
    Seq.unfold (fun pw -> Some (pw, next pw)) seed

let validPasswords seed = 
    seed
    |> passwordCandidates 
    |> (Seq.filter validPassword)

let myInput = "cqjxjnds"
validPasswords myInput |> Seq.take 2 |> List.ofSeq