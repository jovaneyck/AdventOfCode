import Test.HUnit
import Data.Char

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

tests = test [ 
    "react example" ~: "dabCBAcaDA" ~=? react "dabAcCaCBAcCcaDA"
    ]

run = do
    input <- readFile "input.txt"
    putStr "Part1: "
    print $ part1 input
:}

runTestTT tests
run