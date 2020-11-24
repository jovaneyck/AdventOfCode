#r @"..\..\dependencies\Jo\.paket\packages\Unquote\lib\net45\Unquote.dll"
#r @"..\..\dependencies\Jo\.paket\packages\\FParsec\lib\netstandard1.6\FParsecCS.dll"
#r @"..\..\dependencies\Jo\.paket\packages\\FParsec\lib\netstandard1.6\FParsec.dll"
open Swensen.Unquote
open FParsec

type Direction = N | E | S | W
type Regex =
    | Direction of Direction
    | Sequence of Regex list
    | Branch of Regex list
    
let parser =
    let parser, parserRef = createParserForwardedToRef()
    let pbranch = (between (pchar '(') (pchar ')') (sepBy parser (pchar '|'))) |>> Branch
    let pdir =
        [pchar 'N' >>% Direction N
         pchar 'E' >>% Direction E
         pchar 'S' >>% Direction S
         pchar 'W' >>% Direction W]
         |> choice
    let psequence = many (pdir <|> pbranch) |>> Sequence
    do parserRef := psequence

    between (pchar '^') (pchar '$') parser .>> eof

let parse text =
    match run parser text with
    | ParserResult.Success (result,_,_) -> result
    | failure -> failwithf "%A" failure

let tt () = 
    test <@ parse "^N$" = Sequence [ Direction N ] @>
    test <@ parse "^NESW$" = Sequence [Direction N; Direction E; Direction S; Direction W] @>
    test <@ parse "^(N|E)$" = Sequence [Branch [Sequence [Direction N]; Sequence [Direction E]]] @>
    test <@ parse "^(NS|E|SS)$" = Sequence [Branch [Sequence [Direction N; Direction S]; Sequence [Direction E]; Sequence [Direction S; Direction S]]] @>
    test <@ parse "^(N|(E|S)|W)$" = Sequence [Branch [Sequence [Direction N]; Sequence [Branch [Sequence [Direction E]; Sequence [Direction S]]]; Sequence [Direction W]]] @>
    test <@ parse "^(N|)$" = Sequence [Branch [Sequence [Direction N]; Sequence []]] @>
    test <@ parse "^N(E|S)$" = Sequence [Direction N; Branch [Sequence [Direction E]; Sequence [Direction S]]] @>

tt ()

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let parsed = parse input