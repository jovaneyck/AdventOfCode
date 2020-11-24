import qualified Data.Set as Set

parse :: String -> Int
parse ('+':nb) = read nb
parse neg = read neg

frequencies :: [String] -> [Int]
frequencies = 
    scanl (+) 0
    . cycle
    . map parse
        
firstDuplicate :: Set.Set Int -> [Int] -> Int
firstDuplicate visited (x:xs)
    | x `Set.member` visited = x
    | otherwise = firstDuplicate (Set.insert x visited) xs

main = do
    input <- readFile "input.txt"
    let freqs = frequencies $ lines input
    let result = firstDuplicate Set.empty freqs
    putStrLn $ show result