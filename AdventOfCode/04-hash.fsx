let secret = "ckczppom"

let hash (input : string) =
    use md5 = System.Security.Cryptography.MD5.Create()
    input
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let allNumbers = Seq.initInfinite (fun x -> x + 1)
let x = allNumbers |> Seq.take 5 |> List.ofSeq

let isMagicNumber secret number = 
    let hashed = hash (secret + number.ToString())
    hashed.StartsWith "000000"

let magic_number secret =
    allNumbers 
    |> Seq.tryFind (isMagicNumber secret)

magic_number secret