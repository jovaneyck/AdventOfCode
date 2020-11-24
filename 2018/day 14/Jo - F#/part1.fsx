#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
#r @"..\..\dependencies\Jo\.paket\packages\FSharpx.Collections\lib\net40\Fsharpx.Collections.dll"
open Swensen.Unquote
open FSharpx.Collections

let pv = PersistentVector.ofSeq

type State = { recipes : PersistentVector<int>; firstElf : int; secondElf : int}
let initialState = { recipes = [3;7] |> pv; firstElf = 0; secondElf = 1 }

let wrap max index = index % max

let digits combined = 
    if combined < 10 then [combined]
    else [combined / 10; combined % 10]

let tick state =
    let length = state.recipes |> PersistentVector.length    

    let firstRecipe = state.recipes |> PersistentVector.nth (wrap length state.firstElf)
    let secondRecipe = state.recipes |> PersistentVector.nth (wrap length state.secondElf)
    let combined = firstRecipe + secondRecipe
    let newRecipes = digits combined
    let newLength = length + (newRecipes |> Seq.length)
    
    { state with
        recipes = newRecipes |> List.fold (fun vec nr -> PersistentVector.conj nr vec) state.recipes
        firstElf = wrap newLength (state.firstElf + 1 + firstRecipe)
        secondElf = wrap newLength (state.secondElf + 1 + secondRecipe) }

let rec repeat nb f x =
    if nb = 0 then x
    else repeat (nb - 1) f (f x) 

let rec repeatUntilNb nb f state =
    let l = state.recipes |> Seq.length
    let iteration = nb - l
    if iteration % 1000 = 0 then printfn "%d" iteration
    if l >= nb then state
    else repeatUntilNb nb f (f state) 

let solve nbRecipesToMake = 
    let upperBound = (nbRecipesToMake + 10)
    let state = repeatUntilNb upperBound tick initialState 
    let relevant = state.recipes |> Seq.skip nbRecipesToMake |> Seq.take 10
    relevant |> Seq.map string |> String.concat ""

let tt () = 
    printf "Testing..."
    test <@ wrap 3 0 = 0 @>
    test <@ wrap 3 1 = 1 @>
    test <@ wrap 3 2 = 2 @>
    test <@ wrap 3 3 = 0 @>
    test <@ wrap 3 4 = 1 @>
    test <@ digits 10 |> Seq.toList = [1;0] @>
    test <@ digits 5 |> Seq.toList = [5] @>
    test <@ digits 16 |> Seq.toList = [1;6] @>
    test <@ tick initialState = { recipes = [3;7;1;0] |> pv; firstElf = 0; secondElf = 1 } @>
    test <@ repeat 15 tick initialState = {recipes = [3; 7; 1; 0; 1; 0; 1; 2; 4; 5; 1; 5; 8; 9; 1; 6; 7; 7; 9; 2] |> pv; firstElf = 8; secondElf = 4;} @>
    test <@ solve 5 = "0124515891" @>
    test <@ solve 9 = "5158916779" @>
    test <@ solve 18 = "9251071085" @>
    test <@ solve 2018 = "5941429882" @>
    printfn "..done!"

tt ()

let result = solve 505961