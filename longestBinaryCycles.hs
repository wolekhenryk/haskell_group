binaryCycleLength :: Int -> Int
binaryCycleLength n = findCycleLength 1
  where
    findCycleLength k
      | k > 10000000000000     = 0
      | (2^k - 1) mod n == 0 = k
      | otherwise              = findCycleLength (k + 1)

-- Funkcja znajdująca liczbę n z najdłuższym okresem binarnym


longestBinaryCycles :: Int -> [(Int,Int)]
longestBinaryCycles n = [ (k, binaryCycleLength k) | k <- [1,3..n], binaryCycleLength k  == maxCycleLength]
  where
    lengths = map binaryCycleLength [1,3..n]
    maxCycleLength = maximum lengths
  


main :: IO ()
main = do
    putStrLn "Enter the limit:"
    userInput <- getLine
    let limit = read userInput :: Int
    let results = longestBinaryCycles limit
    putStrLn "Liczby od 1 do n z najdłuższymi cyklami binarnymi:"
    mapM_ (\(num, length) -> putStrLn $ "" ++ show num ++ " : " ++ show length) results