sumOfDigitPowers :: Int -> Int -> Int
sumOfDigitPowers power number = sum $ map (^power) $ map (\c -> fromEnum c - fromEnum '0') $ show number

findNumbers :: Int -> [Int]
findNumbers n = [number | number <- [10..10^(n+1)], number == sumOfDigitPowers n number]

main :: IO ()
main = do
    putStrLn "Podaj n:"
    n <- readLn
    let resultNumbers = findNumbers n
    if not (null resultNumbers)
        then do
            putStrLn $ "Liczby spełniające warunki dla n=" ++ show n ++ ": " ++ show resultNumbers
            putStrLn $ "Suma znalezionych liczb: " ++ show (sum resultNumbers)
        else putStrLn $ "Brak liczb spełniających warunki dla n=" ++ show n ++ "."