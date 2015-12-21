open System

let rawInput = @"Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2
Sprinkles: capacity -3, durability 3, flavor 0, texture 0, calories 9
Candy: capacity -1, durability 0, flavor 4, texture 0, calories 1
Chocolate: capacity 0, durability 0, flavor -2, texture 2, calories 8"

type Ingredient = {Name : string; Capacity : int; Durability : int; Flavor : int; Texture : int; Calories : int;}
type Score = {Capacity : int; Durability : int; Flavor : int; Texture : int; Calories : int;}
let zeroScore = {Capacity = 0; Durability = 0; Flavor = 0; Texture = 0; Calories = 0}
type Quantity = int
type RecipeStep = (Ingredient * Quantity)
type Recipe = RecipeStep list

let parseLine (l : string) =
    let nameAndDetails = l.Split([|": "|], StringSplitOptions.None)
    let ingredient = nameAndDetails.[0]
    let parsedProperties =
        nameAndDetails.[1].Split([|","|], StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map (fun prop -> prop.Split(' ') |> Array.last |> System.Int32.Parse)
    match parsedProperties with
    | cap :: dur :: flav :: tex :: cal :: [] -> {Name = ingredient; Capacity = cap; Durability = dur; Flavor = flav; Texture = tex; Calories = cal }
    | _ -> failwith "Invalid input"

let parse (input : string) =
    input.Split('\n')
    |> List.ofArray
    |> List.map parseLine

let parsedInput = 
    rawInput
    |> parse

let nbIngredients = parsedInput |> List.length

let rec allDistributions nb max =
   if nb = 1 then [[max]]
   else
    [0..max]
    |> List.collect(
        fun r -> 
            allDistributions (nb - 1) (max - r)
            |> List.map(fun l -> r :: l))

let rec takeMeasure ingredients distribution : Recipe = 
    match (ingredients, distribution) with
    | [], [] -> []
    | i::it, d :: dt ->  (i, d) :: (takeMeasure it dt)
    | _ -> failwith "Programmer's mistake: expected a number of teaspoons for each ingredient"

let calculateScore acc s = 
    let ({Capacity = cap; Durability = dur; Flavor = flav; Texture = text; Calories = cal; Name = _} : Ingredient, qty) = s
    {acc with 
        Capacity = acc.Capacity + qty * cap
        Durability = acc.Durability + qty * dur
        Flavor = acc.Flavor + qty * flav
        Texture = acc.Texture + qty * text
        Calories = acc.Calories + qty * cal
    } : Score

let score recipe = 
    recipe
    |> List.fold calculateScore zeroScore

let totals ({Capacity = cap; Durability = d; Flavor = f; Texture = t} : Score) =
    [cap;d;f;t] 
    |> List.map(fun value -> 
        if value < 0 
        then 0 
        else value) 
    |> List.reduce (*)
    
allDistributions nbIngredients 100
|> List.map (fun dist -> takeMeasure parsedInput dist)
|> List.map (fun el -> (el, el |> score))
|> List.filter (fun (el, score) -> score.Calories = 500)
|> List.map (fun (el, score) -> (el, score, score |> totals))
|> List.maxBy (fun (el, score, total) -> total)