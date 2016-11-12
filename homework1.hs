-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits num
  | num < 1 = []
  | otherwise = toDigits (num `div` 10) ++ [(num `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev num = rev $ toDigits num

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- Exercise 2
doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft [x] = [x]
doubleEveryOtherLeft (x:y:z) = x : y * 2 : doubleEveryOtherLeft z

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther nums = rev . doubleEveryOtherLeft $ rev nums

--Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits nums = sum $ concatMap toDigits nums

--Exercise 4
validate :: Integer -> Bool
validate num = (\x -> x == 0) . (\x -> x `mod` 10) . sumDigits . doubleEveryOther $ toDigits num

type Peg = String

type Move = (Peg, Peg)

-- hanio:: Integer -> Peg -> Peg -> Peg -> [Move]