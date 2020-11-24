#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote  

let parse lines = 
    seq {
        for y,row in (lines |> Seq.indexed) do
        for x,cell in (row |> Seq.indexed) do
        if cell = '#' then yield (x,y)
    }
    |> List.ofSeq

let angle (x1,y1) (x2,y2) =
    let (dx,dy) = (x2 - x1, y2 - y1)
    atan2 (float dy) (float dx)

let solve input =
    let asteroids = parse input
    
    asteroids
    |> List.map (fun a -> a, asteroids |> List.except [a] |> List.groupBy (angle a))
    |> List.map (fun (a,dirs) -> a,dirs |> Seq.length)
    |> List.maxBy snd

let t () =
    printf "Testing..."

    test <@ 
            let example = [   
                ".#..#"
                "....."
                "#####"
                "....#"
                "...##"]
            solve example = ((3,4), 8) @>

    test <@
            let example = [
                ".#..##.###...#######"
                "##.############..##."
                ".#.######.########.#"
                ".###.#######.####.#."
                "#####.##.#.##.###.##"
                "..#####..#.#########"
                "####################"
                "#.####....###.#.#.##"
                "##.#################"
                "#####.##.###..####.."
                "..######..##.#######"
                "####.##.####...##..#"
                ".#####..#.######.###"
                "##...#.##########..."
                "#.##########.#######"
                ".####.#.###.###.#.##"
                "....##.##.###..#####"
                ".#.#.###########.###"
                "#.#.#.#####.####.###"
                "###.##.####.##.#..##" ]

            solve example = ((11,13),210) @>

    printfn "..done!"
t ()

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> List.ofSeq
input |> solve //((19, 14), 274)