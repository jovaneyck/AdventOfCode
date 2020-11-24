parse :: String -> Int
parse ('+':nb) = read nb
parse neg = read neg

main = do
    input <- readFile "input.txt"
    let solution = sum . map parse . lines $ input
    putStrLn $ show solution