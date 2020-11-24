import Test.HUnit
import Data.Char(toUpper)

:{    
    
react polymer =
    foldr react' "" polymer
    where 
        react' :: Char -> String -> String
        react' el [] = [el]
        react' el (a:as)
            | el /= a && toUpper el == toUpper a = as
            | otherwise = el:a:as

part1 = length . react
part2 polymer =
    let alphabet = ['a'..'z']
        removedUnits = map (\u -> (u, filter (\c -> not $ elem c [u, toUpper u]) polymer)) alphabet
        reacted = map (\(u, p) -> (u, react p)) removedUnits
    in minimum $ map (length . snd) reacted
        

tests = test [ 
    "react example" ~: "dabCBAcaDA" ~=? react "dabAcCaCBAcCcaDA"
    ]

run = do
    input <- readFile "input.txt"
    putStr "Part 1: "
    print $ part1 input
    putStr "Part 2: "
    print $ part2 input
:}

runTestTT tests
run