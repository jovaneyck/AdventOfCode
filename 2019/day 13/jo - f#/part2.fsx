#load "IntCode.fsx"
open IntCode
//IntCode.t ()

let parse output =
    let parsed =
        output 
        |> List.chunkBySize 3
        |> List.map (fun [x;y;o] -> (x,y),o)
    let score = parsed |> List.tryFind (fun ((x,y),_) -> x = -1L && y = 0L)
    match score with
    | None -> None, parsed |> Map.ofList
    | Some ((_, s) as sc) -> Some s, parsed |> List.except [sc] |> Map.ofList

let render (score, grid) : unit =
    printfn "SCORE=%A" score
    let xMax = grid |> Map.toList |> List.map (fst >> fst) |> List.max
    let yMax = grid |> Map.toList |> List.map (fst >> snd) |> List.max
    seq {
        for y in [0L..yMax] do
            printfn ""
            for x in [0L..xMax] do
                let pixel = grid |> Map.find (x,y)
                let sprite =
                    match pixel with
                    | 0L -> " "
                    | 1L -> "W"
                    | 2L -> "B"
                    | 3L -> "_"
                    | 4L -> "O"
                    | u -> failwithf "UNKNOWN PIXEL %A" u
                printf "%s" sprite
    } |> Seq.toList |> ignore
    printfn ""

let merge (old_score, screen) (new_score, updates) =
    let new_screen =
        updates
        |> Map.toList
        |> List.fold (fun s el -> s |> Map.add (fst el) (snd el)) screen
    let new_score = match new_score with | Some s -> Some s | _ -> old_score
    (new_score, new_screen)

type State = { programState : IntCode.State; screen : int64 option * Map<(int64*int64),int64> }
let next state =
    let step = IntCode.run state.programState
    let output = step.output |> List.rev
    let parsedOut = output |> parse 
    let updatedScreen = merge state.screen parsedOut
    let n = { programState = step; screen = updatedScreen }
    render updatedScreen |> ignore
    n

let rec loop (ai : State -> int64) state = 
    let input = ai state
    let n = next { state with programState = { state.programState with output = []; input = [input]; mode = IntCode.Running } }
    match n.programState.mode with
    | IntCode.IntCode.Finished -> printfn "DONE. GOODBYE"
    | _ -> loop ai n

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : IntCode.Program = input.Split([|','|]) |> Seq.map int64 |> Seq.toList

let freeMode = { programState = (IntCode.initState [] (2L :: (p |> List.tail))); screen = Some 0L, Map.empty }
let n = next freeMode

let interactive state =
    printfn "NEXT INPUT:"
    let i = System.Console.ReadLine() //ReadKey does not work in fsi
    
    match i with
    | "q" -> -1L
    | "z" -> 0L
    | "d" -> 1L
    | u -> failwithf "UNKNOWN INPUT %s" u


let ai state =
    let padX = state.screen |> snd |> Map.toList |> List.find (fun (_, e) -> e = 3L) |> fst |> fst
    let ballX = state.screen |> snd |> Map.toList |> List.find (fun (_, e) -> e = 4L) |> fst |> fst
    
    if padX = ballX then 0L
    elif padX < ballX then 1L
    else -1L

//loop interactive n
//loop ai n