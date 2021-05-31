--Prática 03 de Haskell
--Nome: Augusto Pagnossim Frigo

add10toall :: [Int] -> [Int]
add10toall number = [x + 10 | x <- number]

multN :: Int -> [Int] -> [Int]
multN n list = [x*n | x <- list]

multN' :: Int -> [Int] -> [Int]
multN' n list = map (\listAux -> listAux*n) list

applyExpr :: [Int] -> [Int]
applyExpr n = [3*x+2 | x <- n]

applyExpr' :: [Int] -> [Int]
applyExpr' n = map (\aux -> 3*aux+2) n

addSuffix :: String -> [String] -> [String]
addSuffix sufix list = [str ++ sufix | str <- list]

selectgt5 :: [Int] -> [Int]
selectgt5 list = [x | x <- list, x >5]

sumOdds :: [Int] -> Int
sumOdds list = sum [x | x <- list, odd x]

sumOdds' :: [Int] -> Int
sumOdds' list = sum (filter(\aux -> odd aux) list)

selectExpr :: [Int] -> [Int]
selectExpr list = [x | x <- list, x >=20, x <=50, even x]

countShorts :: [String] -> Int
countShorts list = length [x | x <- list, (length x) < 5]

calcExpr :: [Float] -> [Float]
calcExpr numbers = [(x^2)/2 | x <- numbers, (x^2)/2 > 10]

trSpaces :: String -> String
trSpaces word = [if x == ' ' then '-' else x | x <- word]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd list = [snd x | x <- list]

dotProd :: [Int] -> [Int] -> Int
dotProd list1 list2 = sum [fst x * snd x| x <-zip list1 list2]



