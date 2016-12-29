open System
open System.Security.Cryptography

let hasher = MD5.Create()
let hash (word : string) =
    let bytes = System.Text.Encoding.ASCII.GetBytes(word)
    let hash = hasher.ComputeHash(bytes)
    BitConverter.ToString(hash).Replace("-", "")

let indexes = 
    Seq.initInfinite id

let appendTo word suffix = 
    word + suffix

let startsWith prefix (word : string) = 
    word.StartsWith(prefix)

let interestingHashes doorID =
    indexes
    |> Seq.map (string >> appendTo doorID >> hash)
    |> Seq.filter (startsWith "00000")

let nth n (word : string) = 
    word.[n - 1]

let parseHexStringToInt (hex : string) = 
    let hexBase = 16
    Convert.ToInt32(hex, hexBase)

let indexAndRelevantCharacter hash =
    let idx = 
        nth 6 hash 
        |> string 
        |> parseHexStringToInt
    let theCharacter = nth 7 hash
    
    (idx, theCharacter)

let between min max value =
    value >= min && value < max

let accumulateFirstHits accumulator (index, crackedChar) =
    if accumulator |> Map.containsKey index then
        accumulator
    else
        accumulator |> Map.add index crackedChar

let crack passwordLength candidates =
    candidates
    |> Seq.scan accumulateFirstHits Map.empty
    |> Seq.find (fun acc -> acc |> Seq.length = passwordLength)

let indexesWithCharactersToString indexesWithCharacters =
    indexesWithCharacters
    |> Seq.sortBy fst
    |> Seq.map (snd >> string)
    |> String.concat ""

let toLower (text : string) =
    text.ToLower()

let findCode doorID =
    let length = Seq.length doorID

    doorID
    |> interestingHashes
    |> Seq.map indexAndRelevantCharacter
    |> Seq.filter (fst >> between 0 length)
    |> crack length
    |> Map.toSeq
    |> indexesWithCharactersToString
    |> toLower

let secret = "reyedfim"
#time
let solution = findCode secret