import Test.HUnit
import Data.Map(fromListWith, toList)

:{

letterCounts word = toList $ fromListWith (+) [(c, 1) | c <- word]

containsCount nb counts = any ((==) nb) $ map snd counts

solve words = 
    let counted = map letterCounts words
        nbTwos = length $ filter (containsCount 2) counted
        nbThrees = length $ filter (containsCount 3) counted
    in
        nbTwos * nbThrees

example = [ "abcdef"
          , "bababc"
          , "abbcde"
          , "abcccd"
          , "aabcdd"
          , "abcdee"
          , "ababab" ]

tests = test [ 
    "counts" ~: [('e',1),('h',1),('l',2),('o',1)] ~=? counts "hello",
    "example" ~: 12 ~=? solve example
    ]

run = do
    input <- lines <$> readFile "input.txt"
    putStrLn $ "Result: " ++ (show $ solve input)

:}

runTestTT tests
run