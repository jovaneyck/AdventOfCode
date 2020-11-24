let b = 107900
let c = 124900

let isPrime (n : int) = 
    [2..(int (sqrt (float n)))] |> List.forall (fun el -> n % el <> 0)

let part2 = 
    [b .. 17 .. c ]
    |> Seq.filter (not << isPrime)
    |> Seq.length