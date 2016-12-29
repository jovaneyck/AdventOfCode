#r @"lib\Unquote.dll"
#load "RoomChecker.fsx"

open Swensen.Unquote
open RoomChecker

printfn "Testing..."

test <@ shift 1 'a' = 'b' @>
test <@ shift 1 'z' = 'a' @>

test <@ "topsecretchocolatemanagement" = ("gbc-frperg-pubpbyngr-znantrzrag-377[rgbnp]" |> parse |> decrypt) @>

printfn "Tests done!"

let input="gbc-frperg-pubpbyngr-znantrzrag-377[rgbnp]
nij-mywlyn-wlsiayhcw-jfumncw-alumm-mbcjjcha-422[mcjwa]
kloqemlib-lygbzq-pqloxdb-991[lbqod]
veqtekmrk-tpewxmg-kveww-hiwmkr-282[ekwmr]
rflsjynh-ytu-xjhwjy-jll-knsfshnsl-333[jlshn]"

let roomNames =
    input.Split([|'\n'|], System.StringSplitOptions.None)
    |> List.ofSeq

let (roomName, theRoom) =
    roomNames
    |> Seq.map parse
    |> Seq.filter realRoom
    |> Seq.map (fun room -> (decrypt room, room))
    |> Seq.find (fun (name, _) -> name.ToLower().Contains("pole"))

printfn "%s is located in %d" roomName theRoom.Zone