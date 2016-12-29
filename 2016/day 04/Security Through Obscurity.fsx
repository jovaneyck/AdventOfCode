#r @"lib\Unquote.dll"
#load "RoomChecker.fsx"

open Swensen.Unquote
open RoomChecker

printfn "Tests are GO"
//Would normally write ADT functions to avoid ripping into internal data types, but YOLO!
test <@ (parse "aaaaa-bbb-z-y-x-123[abxyz]").Name = ['a';'a';'a';'a';'a';'b';'b';'b';'z';'y';'x'] @>
test <@ (parse "aaaaa-bbb-z-y-x-123[abxyz]").Checksum = ['a';'b';'x';'y';'z'] @>
test <@ (parse "aaaaa-bbb-z-y-x-123[abxyz]").Zone = 123 @>

test <@ rank 3 ['c';'b';'b';'a'] = ['b';'a';'c'] @>
test <@ rank 5 ['a';'a';'a';'a';'a';'b';'b';'b';'z';'y';'x'] = ['a';'b';'x';'y';'z'] @>

let isReal = parse >> realRoom

//Acceptance tests
test <@ isReal "aaaaa-bbb-z-y-x-123[abxyz]" @>
test <@ isReal "a-b-c-d-e-f-g-h-987[abcde]" @>
test <@ isReal "not-a-real-room-404[oarel]" @>
test <@ not <| isReal "totally-real-room-200[decoy]" @>

let myInput = "gbc-frperg-pubpbyngr-znantrzrag-377[rgbnp]
nij-mywlyn-wlsiayhcw-jfumncw-alumm-mbcjjcha-422[mcjwa]
pualyuhapvuhs-ibuuf-zhslz-227[uhalp]
xlrypetn-prr-lylwjdtd-665[dzoya]
zilqwikbqdm-rmttgjmiv-mvoqvmmzqvo-278[mqvio]"

let roomNames =
    myInput.Split([|'\n'|], System.StringSplitOptions.None)
    |> List.ofSeq

let solution = 
    sumOfValidRoomZones roomNames