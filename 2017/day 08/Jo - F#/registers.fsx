#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

let mutable maximum = System.Int32.MinValue //forgive me, FP gods

type Register = string
type Value = int
type Action = Inc | Dec
type Operator = GT | LT | GE | LE | EQ | NE
type Condition = {register : Register; operator : Operator; threshold : Value}
type Instruction = { register : Register; action : Action; value : Value; condition : Condition }

let split (sep : char) (text : string) = text.Split([|sep|])

let parseInstruction line = 
    let tokens = line |> split ' '
    let parseAction =
        function
        | "inc" -> Inc
        | "dec" -> Dec
        | notsupported -> failwithf "Action %s is not supported." notsupported
    let parseValue = System.Int32.Parse
    let parseOperator =
        function
        | ">" -> GT
        | "<" -> LT
        | ">=" -> GE
        | "<=" -> LE
        | "==" -> EQ
        | "!=" -> NE
        | unrecognized -> failwithf "Did not recognize comparison operator %s" unrecognized

    let condition = 
        {
            register = tokens.[4]
            operator = parseOperator tokens.[5]
            threshold = parseValue tokens.[6]
        }

    {
        register = tokens.[0]
        action = parseAction tokens.[1]
        value = parseValue tokens.[2]
        condition = condition
    }

let parse text = 
    text
    |> split '\n'
    |> Seq.map parseInstruction

let value registers register =
    match registers |> Map.tryFind register with
    | None -> 0 //default value
    | Some value -> value

let evaluate registers (condition : Condition) = 
    let eval a op b = 
        match op with
        | EQ -> a = b
        | NE -> a <> b
        | LT -> a < b
        | GT -> a > b
        | LE -> a <= b
        | GE -> a >= b
    let registerValue = value registers condition.register
    eval registerValue condition.operator condition.threshold

let apply registers instruction =
    let oldValue = value registers instruction.register
    let newValue = 
        match instruction.action with
        | Inc -> oldValue + instruction.value
        | Dec -> oldValue - instruction.value

    if newValue > maximum then
        maximum <- newValue

    registers |> Map.add instruction.register newValue

let runInstruction registers instruction = 
    if not (evaluate registers instruction.condition) then
        registers
    else 
        apply registers instruction

let run program = 
    program
    |> Seq.fold runInstruction Map.empty

let max registers = 
    registers 
    |> Map.toSeq
    |> Seq.maxBy snd

let example = @"b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"

printf "Testing..."
test <@ parseInstruction "b inc 5 if a > 1" = {register = "b"; action = Inc; value = 5; condition = {register = "a"; operator = GT; threshold = 1}} @>
test <@ parseInstruction "cc dec -20 if de == -30" = {register = "cc"; action = Dec; value = -20; condition = {register = "de"; operator = EQ; threshold = -30}} @>

test <@ evaluate ([("a", 10)] |> Map.ofSeq) {register = "a"; operator = EQ; threshold = 10} @>
test <@ not <| evaluate ([("a", 10)] |> Map.ofSeq) {register = "a"; operator = EQ; threshold = 9} @>
test <@ not <| evaluate ([("a", 10)] |> Map.ofSeq) {register = "b"; operator = EQ; threshold = 10} @>
test <@ evaluate ([("a", 10)] |> Map.ofSeq) {register = "b"; operator = EQ; threshold = 0} @>
printfn "..done!"

//Acceptance test
test <@ 1 = (example |> parse |> run |> max |> snd) @>

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
input |> parse |> run |> max