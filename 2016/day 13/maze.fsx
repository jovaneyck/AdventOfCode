#r "..\..\0 lib\F#\Unquote.dll"
open Swensen.Unquote

let numberOfOnesInBinaryRepresentation number = 
    let rec bin acc decimal =
        if decimal = 0 then
            acc
        else
            let nextDigit = decimal % 2
            let nextAcc = 
                match nextDigit with
                | 0 -> acc
                | 1 -> 1 + acc
                | _ -> failwith "NOPE. Nothing is true. Everything is permitted."
            bin nextAcc (decimal / 2)
    bin 0 number

let isEven n = n % 2 = 0
let isOpen fav (x, y) = 
    fav + (x*x + 3*x + 2*x*y + y + y*y)
    |> numberOfOnesInBinaryRepresentation
    |> isEven

let neighbours (x,y) =
     [(x+1, y); (x, y+1) ; (x-1, y); (x, y-1)]
     |> List.filter (fun (x,y) -> x >= 0 && y >= 0)

let neighboursWithDistance fav src =
    let (distanceToCurrent, current) = src
    let newDistance = distanceToCurrent + 1

    current 
    |> neighbours
    |> List.filter (isOpen fav)
    |> List.map (fun n -> (newDistance, n))

let solution dest hits =
    hits
    |> List.tryFind (fun (_,location) -> location = dest)

let sortAndPrune elements = 
    elements
    |> List.distinct
    |> List.sortBy fst

let lengthOfShortestPath fav dest = 
    let rec sp queue =
        match queue with
        | [] -> failwith "Nope."
        | h :: t -> 
            let neighbours = neighboursWithDistance fav h
            match solution dest neighbours with
            | Some (dst,_) -> 
                dst
            | None -> 
                sp <| sortAndPrune (neighbours @ t)
    sp [(0,(1,1))]

let vistedNodesOfMaxDistance fav max = 
    let rec sp acc queue =
        match queue with
        | [] -> failwith "Nope."
        | h :: t -> 
            let (currentdistance, _) = h
            if currentdistance = max then
                acc |> List.map snd |> List.distinct
            else
                let neighbours = neighboursWithDistance fav h
                sp (neighbours @ acc) (sortAndPrune (neighbours @ t))
    sp [(0,(1,1))] [(0,(1,1))]

let favoriteNumber = 10
printfn "Testing.."
test <@ [(0,0);(9,2);(7,4)] |> Seq.map (isOpen favoriteNumber) |> Seq.forall id @>
test <@ [(1,0);(5,2);(9,6)] |> Seq.map (isOpen favoriteNumber) |> Seq.forall (id >> not) @>
test <@ neighbours (0,0) = [(1,0); (0,1)] @>
test <@ neighbours (1,1) = [(2, 1); (1, 2); (0, 1); (1, 0)]  @>
test <@ lengthOfShortestPath favoriteNumber (7,4) = 11 @>
printfn "Tests done!"

#time
let pt1 = lengthOfShortestPath 1350 (31,39)
let pt2 = vistedNodesOfMaxDistance 1350 50 |> List.length