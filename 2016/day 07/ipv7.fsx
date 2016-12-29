#r ".\lib\Unquote.dll"
open Swensen.Unquote
open System.Text.RegularExpressions

let debug = (fun x -> printfn "DEBUG: %A" x; x)    

type IPAddress = { parts : char list list; hypernets : char list list }

let parse (iptext : string) =
    let allParts = iptext.Split([|'[';']'|]) |> List.ofSeq
    let (evens, odds) = 
        allParts 
        |> List.indexed 
        |> List.partition (fun (index, part) -> index % 2 = 0)
    { parts = evens |> List.map (snd >> List.ofSeq); hypernets = odds |> List.map (snd >> List.ofSeq) }

let containsABBA ipPart = 
    ipPart
    |> List.windowed 4
    |> List.exists (function | [a;b;c;d] -> a = d && b = c && a <> b | _ -> false)

let supportsTLS ip = 
    let anyABBA = List.exists containsABBA
    (ip.parts |> anyABBA) && not (ip.hypernets |> anyABBA)

test <@ parse "ab[cd]ef" = {parts = [['a';'b'];['e';'f']]; hypernets = [['c';'d']]} @>
test <@ parse "ab[cd]ef[gh]ij[kl]mn" = {parts = [['a';'b'];['e';'f'];['i';'j'];['m';'n']]; hypernets = [['c';'d'];['g';'h'];['k';'l']]} @>
test <@ "abba[mnop]qrst" |> parse |> supportsTLS @>
test <@ not ("abcd[bddb]xyyx" |> parse |> supportsTLS)@>
test <@ not ("aaaa[qwer]tyui" |> parse |> supportsTLS)@>
test <@ "ioxxoj[asdfgh]zxcvbn" |> parse |> supportsTLS@>

let input = "rhamaeovmbheijj[hkwbkqzlcscwjkyjulk]ajsxfuemamuqcjccbc
gdlrknrmexvaypu[crqappbbcaplkkzb]vhvkjyadjsryysvj[nbvypeadikilcwg]jwxlimrgakadpxu[dgoanojvdvwfabtt]yqsalmulblolkgsheo
dqpthtgufgzjojuvzvm[eejdhpcqyiydwod]iingwezvcbtowwzc[uzlxaqenhgsebqskn]wcucfmnlarrvdceuxqc[dkwcsxeitcobaylhbvc]klxammurpqgmpsxsr
gmmfbtpprishiujnpdi[wedykxqyntvrkfdzom]uidgvubnregvorgnhm
txxplravpgztjqcw[txgmmtlhmqpmmwp]bmhfgpmafxqwtrpr[inntmjmgqothdzfqgxq]cvtwvembpvdmcvk
gkxjhpayoyrrpcr[mwyoahlkqyhtznyzrm]mvmurvsrgjunjjepn[mkoumuohilpcfgbmsmh]hpwggyvjkusjxcyojyr[wqxyuzbewpjzlyqmkhw]nniczueulxtdsmkniex
vuzyoofrvaanszwndyt[mzcbhmabgnetrpje]tqnygwhmwrbyosbke[gehqzyhlnyufknqmueo]ngendggbjcvazwol
vdnploylmxnipfudw[pbkxlaozhqhlbzz]kpxnzwjhybgcenyw[fpukiqrjraomftt]rosyxtsdltbsmhykxu[wrucjfwuyypmiic]ydnbgvicfnmwzuatudd
lknaffpzamlkufgt[uvdgeatxkofgoyoi]ajtqcsfdarjrddrzo[bxrcozuxifgevmog]rlyfschtnrklzufjzm
kajqeqlafxtmzirn[mkftybdukmghmyoclxd]plvjnikiozkikifpodt[cmufoktkndkhaeqbztz]drjixnnsdxqnrmn[cmzsnhlirtskunngcee]upgxlcjhmoethppx
joibiixuzgtkjquor[xmnqotlqrhpvlglwaxe]kjmfrpihitjydwda
kouyuiijgsmpzynmt[xvwuujrfkqjmtqdh]ukjscwcnwktrfvrmvew[quzbelbcfxknvqc]drtrmvnabjkslahadad
hhlcltfpiwfjhguif[rpasuqltkbudhwjeew]mkcmvbxqukjczex
xxqceycviwyzqxekn[tiidftrsnlgpesxlf]obtbqfgogpwkoqow[dabhpdntfvbhgtmupy]hbvtghnycgyywavqbtg
zlqdqmuxebccmndzbl[ykefimjzdqdmfvlflj]ptlphteflzxmolkof
babzuaikmedruqsuuv[emlhynmvfhsigdryo]iyblsqlpplrlahtwr
byddropvzudnjciymyh[jcebyxyvikkshpn]ggmrxgkzsrfkfkzo
ektijwczwnlancuqfv[luqhtfgwmlilhwnk]gxgivxlnerdhbhetfz[bzczfdorrsptzikjmct]mfrsvxgxijtusmvjd[sbpnwycbrykuhsinudc]bmpikuskzlxcoidp
igefoemugshofmibco[uhahihzaglmzdpzjvfp]tfbuuhoughgismec[inbtuzxnxekfkulodyk]fxykxfkfnjvswwc"

input.Split('\n')
|> Seq.map parse
|> Seq.filter supportsTLS
|> Seq.length