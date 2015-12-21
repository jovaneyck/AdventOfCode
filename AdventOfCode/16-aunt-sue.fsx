#load "16-aunt-sue-input.fs"

type SueDescription =
    {
        Number : int;
        Children : int Option;
        Cats : int Option;
        Samoyeds : int Option;
        Pomeranians : int Option;
        Akitas : int Option;
        Vizslas : int Option;
        Goldfish : int Option;
        Trees : int Option;
        Cars : int Option;
        Perfumes : int Option;
    }
let zeroDescription =
    {
        Number = 0;
        Children = None;
        Cats = None;
        Samoyeds = None;
        Pomeranians = None;
        Akitas = None;
        Vizslas = None;
        Goldfish = None;
        Trees = None;
        Cars = None;
        Perfumes = None
    }

let parseProperty (propdef : string) =
    let split = propdef.Split(' ')
    (split.[0], split.[1] |> System.Int32.Parse)

let parseLine (l : System.String) =
    let nbAndProps = l.Split(':')
    let nb = nbAndProps.[0].Split(' ').[1] |> System.Int32.Parse
    let props = nbAndProps |> List.ofArray |> List.tail |> List.reduce (+)
    let parsedProps = 
        props.Split([|", "|], System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray 
        |> List.map (fun el -> el.Trim())
        |> List.map parseProperty
    (nb, parsedProps)

let withProperty (property, number) descr = 
    let nb = Some number
    match property with
    | "children" -> {descr with Children = nb}
    | "cars" -> {descr with Cars = nb}
    | "vizslas" -> {descr with Vizslas = nb}
    | "akitas" -> {descr with Akitas = nb}
    | "perfumes" -> {descr with Perfumes = nb}
    | "pomeranians" -> {descr with Pomeranians = nb}
    | "goldfish" -> {descr with Goldfish = nb}
    | "cats" -> {descr with Cats = nb}
    | "trees" -> {descr with Trees = nb}
    | "samoyeds" -> {descr with Samoyeds = nb}
    | unknown -> failwith (sprintf "Could not parse property %s" unknown)

let rec buildSue acc props =
    match props with
    | [] -> acc
    | p :: t -> buildSue (acc |> withProperty p) t

let buildSueDescription (id, props) =
    buildSue {zeroDescription with Number = id} props

let isEqualOrUnknown v =
    function
    | None -> true
    | Some x when x = v -> true
    | _ -> false
let crossReference (description : SueDescription) =
    Some description
    |> Option.filter (fun d -> d.Children |> isEqualOrUnknown 3)
    |> Option.filter (fun d -> d.Cats |> isEqualOrUnknown 7)
    |> Option.filter (fun d -> d.Samoyeds |> isEqualOrUnknown 2)
    |> Option.filter (fun d -> d.Pomeranians |> isEqualOrUnknown 3)
    |> Option.filter (fun d -> d.Akitas |> isEqualOrUnknown 0)
    |> Option.filter (fun d -> d.Vizslas |> isEqualOrUnknown 0)
    |> Option.filter (fun d -> d.Goldfish |> isEqualOrUnknown 5)
    |> Option.filter (fun d -> d.Trees |> isEqualOrUnknown 3)
    |> Option.filter (fun d -> d.Cars |> isEqualOrUnknown 2)
    |> Option.filter (fun d -> d.Perfumes |> isEqualOrUnknown 1)
    
AuntSueInput.input.Split('\n')
|> List.ofArray
|> List.map parseLine
|> List.map buildSueDescription
|> List.choose crossReference