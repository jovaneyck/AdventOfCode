type Gear = int * int * int

let run config =
    Seq.initInfinite id
    |> Seq.find (fun timeTick -> 
        config 
        |> Seq.forall (fun (level,max,start) -> 
            (timeTick + level + start) % max = 0))

let example =
    [
        (1,5,4)
        (2,2,1)
    ]

let pt1 =
    [
        (1,13,1)
        (2, 19, 10) 
        (3, 3, 2)
        (4, 7, 1)  
        (5, 5, 3) 
        (6, 17, 5)
    ]

let pt2 =
    [
        (1, 13, 1)
        (2, 19, 10)
        (3, 3, 2) 
        (4, 7, 1)  
        (5, 5, 3)  
        (6, 17, 5)
        (7, 11, 0)  
    ]

run pt2 |> (printfn "Found it: %A")