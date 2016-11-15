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
validate num = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther $ toDigits num

--Exercise 5

type Peg = String

type Move = (Peg, Peg)

hanoi:: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi disks peg1 peg2 peg3
  | disks < 0 = []
  | disks == 0 = [(peg3, peg2)]
  | otherwise = (peg1, peg3) : hanoi (disks - 1) peg1 peg3 peg2
