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

let isPostfix pattern vector =
    let rec pf pattern vector =
        match pattern with
        | [] -> true
        | x :: xs -> 
            if vector |> PersistentVector.length = 0 then false
            else 
                let rest, last = vector |> PersistentVector.unconj
                if x = last then
                    pf xs rest
                else false
    pf (pattern |> List.rev) vector

let rec removeLast nb pv =
    if nb = 0 then pv
    else
        let (init, _) = PersistentVector.unconj pv
        removeLast (nb-1) init

let repeatUntilPattern pattern f state  =
    let p = pattern |> Seq.toList |> List.map (string >> int)
    let rec loop state =
        let l = PersistentVector.length state.recipes
        if l % 1000 = 0 then printfn "%d" l
        let next = f state
        if isPostfix p next.recipes 
        then next.recipes |> (removeLast (p |> Seq.length)) |> PersistentVector.length
        elif
            (let (init, _) = PersistentVector.unconj next.recipes
            isPostfix p init)
        then
            next.recipes |> (removeLast ((p |> Seq.length) + 1)) |> PersistentVector.length
        else
            loop next
    loop state

let solve (pattern : string) = 
    let result = repeatUntilPattern pattern tick initialState 
    result

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
    test <@ isPostfix [] (pv [1..10]) @>
    test <@ isPostfix [7..10] (pv [1..10]) @>
    test <@ isPostfix [1..10] (pv [1..10]) @>
    test <@ not <| isPostfix [1..3] (pv [1..10]) @>
    test <@ removeLast 0 (pv [1..10]) = pv [1..10] @>
    test <@ removeLast 5 (pv [1..10]) = pv [1..5] @>
    //Acceptance tests
    test <@ solve "1" = 2 @>
    test <@ solve "0" = 3 @>
    test <@ solve "01245" = 5 @>
    test <@ solve "51589" = 9 @>
    test <@ solve "92510" = 18 @>
    test <@ solve "59414" = 2018 @>
    printfn "..done!"

tt ()
    
//let result = solve "505961" //20231866