#r ".\lib\Unquote.dll"
open Swensen.Unquote
open System.Text.RegularExpressions

let debug = (fun x -> printfn "DEBUG: %A" x; x)    

type IPAddress = { supernets : char list list; hypernets : char list list }

let parse (iptext : string) =
    let allParts = iptext.Split([|'[';']'|]) |> List.ofSeq
    let (evens, odds) = 
        allParts 
        |> List.indexed 
        |> List.partition (fun (index, part) -> index % 2 = 0)
    { supernets = evens |> List.map (snd >> List.ofSeq); hypernets = odds |> List.map (snd >> List.ofSeq) }

let rec findABAsInSingle accumulator supernet =
    supernet
    |> List.windowed 3
    |> List.map (function | [a;b;c] -> (a,b,c) | _ -> failwith "Expected a list of three elements")
    |> List.filter (fun (a,b,c) -> a = c && a <> b)

let findABAs supernets =
    supernets
    |> List.collect (findABAsInSingle [])
    |> List.distinct

let toBAB (a,b,c) = (b,a,b) 

let isPresentIn hypernet bab = 
    let (a1,b1,c1) = bab
    hypernet
    |> List.windowed 3
    |> List.exists (function | [a2;b2;c2] -> a1 = a2 && b1 = b2 && c1 = c2 | _ -> false)

let containsAnyBAB babs hypernet =
    babs 
    |> List.exists (isPresentIn hypernet)

let supportsSSL ip = 
    let allABAs = 
        findABAs ip.supernets
    let allBABs = 
        allABAs 
        |> List.map toBAB
    ip.hypernets
    |> List.exists (containsAnyBAB allBABs)

test <@ parse "ab[cd]ef" = {supernets = [['a';'b'];['e';'f']]; hypernets = [['c';'d']]} @>
test <@ parse "ab[cd]ef[gh]ij[kl]mn" = {supernets = [['a';'b'];['e';'f'];['i';'j'];['m';'n']]; hypernets = [['c';'d'];['g';'h'];['k';'l']]} @>

test <@ "aba[bab]xyz" |> parse |> supportsSSL @>
test <@ not ("xyx[xyx]xyx" |> parse |> supportsSSL) @>
test <@ "aaa[kek]eke" |> parse |> supportsSSL @>
test <@ "zazbz[bzb]cdb" |> parse |> supportsSSL @>

let input = "byddropvzudnjciymyh[jcebyxyvikkshpn]ggmrxgkzsrfkfkzo
ektijwczwnlancuqfv[luqhtfgwmlilhwnk]gxgivxlnerdhbhetfz[bzczfdorrsptzikjmct]mfrsvxgxijtusmvjd[sbpnwycbrykuhsinudc]bmpikuskzlxcoidp
igefoemugshofmibco[uhahihzaglmzdpzjvfp]tfbuuhoughgismec[inbtuzxnxekfkulodyk]fxykxfkfnjvswwc
onmmhtsykubbpdiqvjm[kbfbiyjyuzmemaomkwa]prqwqocsihfnslooel[hysggeprqecalydywlk]taghiwhgnujsduhnffu[ibpvowghgttfsvt]wcajwcxhcriflxi
evvhkvndeoxrrftqmih[ckxjgqvpdxjvmbwsor]odolgenlgaxujvqg[qyrnnrjgxskuxycoip]jvtjgwaaywdphxpy
fffaewoawlzsmnqo[ubnpbqpxgenzjiytml]ztberlzwpzdvofcwo
vhrwunprhbpclog[vqtnbjndcwpuyen]vzuudswovzmjviee
yfeztpcfgazkijht[xqcjocbnjmvvrzg]maisokokpukpstgpj
neudpatmnjayamydbrd[heckokdparzefxm]qulfvfivofznkyvkwq[owjrktbaejpffqef]oserqezusmubsertq
ykgyzyqlodjvgqzmzy[ewsxadkknhduejft]yysinlpnxpaqdai[hqagzwigkpvzsje]auibbpljfmkoxaskuh
kntmgvoypnpibjtp[ispxkdofjsdufpivwrj]ndecwlfcbjtrnrzw"

input.Split('\n')
|> Seq.map parse
|> Seq.filter supportsSSL
|> Seq.length