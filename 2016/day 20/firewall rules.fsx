#r "..\..\0 lib\F#\Unquote.dll"
open Swensen.Unquote

open System.IO
open System.Text.RegularExpressions

let parseIP = System.UInt64.Parse

let parseRange r = 
    let m = Regex.Match(r, "(.*)-(.*)")
    (m.Groups.[1].Value |> parseIP, m.Groups.[2].Value |> parseIP)

let parse (ranges : string) = 
    ranges.Split([|'\n'|])
    |> Seq.map parseRange

let inline inRange ip (from, til) =
    from <= ip && ip <= til

let inline inBlackList blacklist ip = 
//    printfn "Checking %d" ip
    blacklist
    |> Seq.exists (inRange ip)

let inline between min max value = min <= value && value <= max

let shrink blacklist = 
    let rec s acc l =
        match l with
        | (f1,t1) :: (f2, t2) :: rest when f2 |> between f1 t1 && t2 |> between f1 t1 -> s ((f1,t1) :: acc) rest
        | (f1,t1) :: (f2, t2) :: rest when f2 |> between f1 t1 && t1 |> between f2 t2 -> s ((f1,t2) :: acc) rest
        | range :: rest -> s (range :: acc) rest
        | [] -> acc
    s [] (blacklist |> List.sort)

let firstWhitelisted max rawInput =
    let blacklist = 
        rawInput
        |> parse
        |> List.ofSeq
        |> shrink |> shrink |> shrink |> shrink |> shrink |> shrink //not in the mood for fixed point right now, validated # of shrinks by hand :)
    seq { for i in 0UL..max -> i} 
    |> Seq.find (inBlackList blacklist >> not)

let example = "5-8\n0-2\n4-7"
test <@ firstWhitelisted 9UL example = 3UL @>

#time
let input = File.ReadAllText(System.IO.Path.Combine([| __SOURCE_DIRECTORY__; "input.txt"|]))

//Part 1
let pt1 = firstWhitelisted 4294967295UL input

//Part 2 
let blacklist = 
    parse input 
    |> List.ofSeq 
    |> shrink |> shrink |> shrink |> shrink |> shrink |> shrink

// this is a bit too slow :)
//seq { for i in 0UL..4294967295UL -> i} |> Seq.filter (inBlackList blacklist >> not) |> Seq.length

let blacklisted = 
    blacklist
    |> Seq.map (fun (from, til) -> til - from + 1UL)
    |> Seq.sum
let pt2 = 4294967295UL + 1UL - blacklisted