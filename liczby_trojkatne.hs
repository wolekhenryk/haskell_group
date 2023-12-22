-- Funkcja generująca liczby trójkątne
triangleNumbers :: [Integer]
triangleNumbers = scanl1 (+) [0..]

-- Funkcja licząca dzielniki liczby
divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n], n mod x == 0]

-- Funkcja licząca długość listy zwracającą Integer
lengthInteger :: [a] -> Integer
lengthInteger = foldr (\_ acc -> acc + 1) 0

-- Funkcja znajdująca pierwszą liczbę trójkątną z więcej niż n dzielnikami
findFirstTriangle :: Integer -> Integer
findFirstTriangle n = head [x | x <- triangleNumbers, lengthInteger (divisors x) > n]

-- Główna funkcja
main :: IO ()
main = do
    putStrLn "Podaj liczbę n:"
    nStr <- getLine
    let n = read nStr :: Integer
    print $ findFirstTriangle n