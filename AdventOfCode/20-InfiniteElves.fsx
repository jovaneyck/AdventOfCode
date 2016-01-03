let houses = Seq.initInfinite (fun el -> el + 1)

let divisors n =
    let sr = (float) n |> sqrt |> (int)
    [sr..(-1)..1]
    |> List.filter (fun h -> h * 50 < n)
    |> List.filter (fun h -> n % h = 0)
    |> List.collect (fun el -> [el; n / el])
    |> List.distinct

let elvesWithAJobFor = divisors
let nbPresents elf = elf * 11

houses
    |> Seq.map (fun h ->
        let nbPresents =
            elvesWithAJobFor h
            |> List.map nbPresents
            |> List.sum
        (h, nbPresents))
    |> Seq.skipWhile (fun (_,p) -> p < 33100000)
    |> Seq.map (fun (h, p) ->
        sprintf "House %d got %d presents." h p)
    |> Seq.take 1
    |> List.ofSeq