import Test.HUnit
import Data.List((\\), intersect)

:{

nbDifferences :: Int -> String -> String -> Int
nbDifferences acc [] [] = acc
nbDifferences acc (x:xs) (y:ys)
    | x /= y = nbDifferences (acc + 1) xs ys
    | otherwise = nbDifferences acc xs ys

countDifferences :: String -> [String] -> (String, [(String, Int)])
countDifferences word dictionary = 
    ( word
    , map (\w -> (w, nbDifferences 0 word w)) dictionary)

deltas :: [String] -> [(String, [(String, Int)])]
deltas words =
    map (\word -> countDifferences word $ words \\ [word]) words

solve :: [String] -> String
solve input = let
    diffs = deltas input
    distance1 = ( head
                . filter (\(_,d) -> d /= []) 
                . map (\(w,deltas) -> (w,  filter ((1 ==) . snd) deltas))) diffs
    (word, [(otherWord,_)]) = distance1
    commonLetters = word `intersect` otherWord
    in commonLetters

example = ["abcde"
          , "fghij"
          , "klmno"
          , "pqrst"
          , "fguij"
          , "axcye"
          , "wvxyz"]

tests = test [ 
    "Counting differences" 
        ~: (0, 1, 3) 
        ~=? ( nbDifferences 0 "abc" "abc"
            , nbDifferences 0 "abc" "adc"
            , nbDifferences 0 "abc" "xyz")
    , "Calculating deltas"
        ~: [("abc",[("xyz",3),("abd",1)])
           ,("xyz",[("abc",3),("abd",3)])
           ,("abd",[("abc",1),("xyz",3)])]
        ~=? deltas ["abc","xyz","abd"]
    ]

run = do
    input <- lines <$> readFile "input.txt"
    let solution = solve input
    putStrLn $ "Solution: " ++ show solution

:}

runTestTT tests
run