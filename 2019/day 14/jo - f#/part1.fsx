open System.Text.RegularExpressions
//every chemical is produced by exactly one reaction; the only exception, ORE


type Quantity = { Amount : int; Chemical : string }
type Reaction = { Input : Quantity list; Output : Quantity }

let parseQuantity input = 
    let m = Regex.Match(input, "(\d*) (.*)")
    { Amount = m.Groups.[1].Value |> int
    ; Chemical = m.Groups.[2].Value
    }

let parse reaction =
    let m = Regex.Match(reaction, "(.*) => (.*)")
    let output = parseQuantity m.Groups.[2].Value
    let inputs =
        m.Groups.[1].Value.Split([|", "|], System.StringSplitOptions.None) 
        |> List.ofSeq 
        |> List.map parseQuantity
    { Input = inputs; Output = output }

let rec cook recipebook stocks ingredients =
    match ingredients with
    | []-> stocks
    | i :: is ->
        //printfn "Cooking %A\nTODO: %A\nstocks: %A\n===" i is stocks
        let stock = stocks |> Map.find i.Chemical
        if stock >= i.Amount then
            //printfn "* Enough stock for %A: %A. Eating it up & proceeding with next step in recipe" i stock
            let new_stocks = stocks |> Map.add i.Chemical (stock - i.Amount)
            cook recipebook new_stocks is
        else
            //printfn "* Not enough stock for %A: %A. Cooking some more" i stock
            let recipe = recipebook |> Map.find i.Chemical
            let need_to_cook = i.Amount - stock
            let batches = (float need_to_cook) / (float recipe.Output.Amount) |> System.Math.Ceiling |> int
            let new_recipes = recipe.Input |> List.map (fun a -> { a with Amount = a.Amount * batches })
            cook recipebook (stocks |> Map.add i.Chemical (batches * recipe.Output.Amount - need_to_cook)) (List.append new_recipes is)

let example = [
    "10 ORE => 10 A"
    "1 ORE => 1 B"
    "7 A, 1 B => 1 C"
    "7 A, 1 C => 1 D"
    "7 A, 1 D => 1 E"
    "7 A, 1 E => 1 FUEL"
    ]

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Seq.toList
let reactions = input |> List.map parse
let recipebook = reactions |> List.map (fun r -> (r.Output.Chemical, r)) |> Map.ofList
let stock = 
    reactions 
    |> List.collect (fun r -> (r.Output.Chemical :: (r.Input |> List.map (fun i-> i.Chemical) )))
    |> List.distinct
    |> List.map (fun i -> (i, 0))
    |> Map.ofList
    |> Map.add "ORE"  System.Int32.MaxValue
    
let ore_stock =
    cook recipebook stock [{ Amount = 1; Chemical = "FUEL" }]
    |> Map.find "ORE"
System.Int32.MaxValue - ore_stock