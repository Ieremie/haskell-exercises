-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList,
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond,
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate,
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence,
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse,
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where

-- Exercise 1
-- split a given list into sub-lists
-- each of these must be strictly ascending, descending, or equal
splitSort :: Ord a => [a] -> [[a]]
splitSort [x] = [[x]]
splitSort [] = []
splitSort ns = init ( ascend'' ns)

ascend'' :: Ord a => [a] -> [[a]]
ascend'' [] = [[]]
ascend'' (x:xs) = [result] ++ ascend'' ( drop (length result -1) xs )
  where
    result = ascend' (x:xs)

ascend' :: Ord a => [a] -> [a]
ascend' [] = []
ascend' [x] = [x]
ascend' (x:y:yz)
  | x > y = head ( ascend1 (x:y:yz) )
  | x < y = head ( ascend2 (x:y:yz) )
  | x == y = head ( ascend3 (x:y:yz) )

ascend1 :: Ord a => [a] -> [[a]]
ascend1 xs = foldr f [] xs
  where
    f a []  = [[a]]
    f a (y:ys) | a > head y = (a:y):ys
               | otherwise = [a]:(y:ys)

ascend2 :: Ord a => [a] -> [[a]]
ascend2 xs = foldr f [] xs
  where
  f a []  = [[a]]
  f a (y:ys) | a < head y = (a:y):ys
             | otherwise = [a]:(y:ys)


ascend3 :: Ord a => [a] -> [[a]]
ascend3 xs = foldr f [] xs
  where
    f a []  = [[a]]
    f a (y:ys) | a == head y = (a:y):ys
               | otherwise = [a]:(y:ys)
-- Exercise 2
-- longest common sub-list of a finite list of finite list
longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList (x:y:[]) = commonDouble x y
longestCommonSubList (x:xs)
  | length (foldl ( commonDouble ) x xs) > length( foldr ( commonDouble ) x xs) = foldl ( commonDouble ) x xs
  | otherwise = foldr ( commonDouble ) x xs

wrapper :: Eq a => [[a]] -> [a]
wrapper [] = []
wrapper (x:y:[]) = commonDouble x y
wrapper (x:xs) = foldl ( commonDouble ) x xs

commonDouble :: Eq a => [a] -> [a] -> [a]
commonDouble [] _ = []
commonDouble _ [] = []
commonDouble (x:xs) (y:ys)
  | x == y = x : commonDouble xs ys
  | otherwise = let
    first = commonDouble (x:xs) ys
    second = commonDouble xs (y:ys)
    in case (length first) > (length second) of
      True -> first
      otherwise -> second

-- Exercise 3
-- check whether the given results are sufficient to pass the year
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
canProgress :: [ModuleResult] -> Bool
canProgress [] = False
canProgress ms
  | addCredits ms >= 60.0 && failure ms == 0 = True
  | addCredits ms < 45.0 = False
  | addCredits ms < 60.0 && addCredits ms >= 45.0 && addCredits1 ms <= 15.0 && canCompensate ms > 0 = True
  | otherwise = False

addCredits :: [ModuleResult] -> Float
addCredits [] = 0.0
addCredits ((ModuleResult c m ):ms)
  | m >= 40 = c + addCredits ms
  | otherwise = addCredits ms

canCompensate :: [ModuleResult] -> Int
canCompensate [] = 0
canCompensate ((ModuleResult c m ):ms)
  | m < 40 && m >= 25 = 1 + canCompensate ms
  | otherwise = canCompensate ms

failure :: [ModuleResult] -> Int
failure [] = 0
failure ((ModuleResult c m ):ms)
  | m < 25 = 1 + failure ms
  | otherwise = failure ms

addCredits1 :: [ModuleResult] -> Float
addCredits1 [] = 0.0
addCredits1 ((ModuleResult c m ):ms)
  | m < 40 && m >= 25 = c + addCredits1 ms
  | otherwise = addCredits1 ms

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
sumOfCredits :: [ModuleResult] -> Float
sumOfCredits [] = 0.0
sumOfCredits ((ModuleResult c m):xs) = c + sumOfCredits xs

sumOfMarks :: [ModuleResult] -> Float
sumOfMarks  [] = 0.0
sumOfMarks ((ModuleResult c m):xs) = (fromIntegral m) * c + sumOfMarks xs

calculateYearAverage :: [ModuleResult] -> [ModuleResult]
calculateYearAverage (x:[]) = [x]
calculateYearAverage xs = [ModuleResult (sumOfCredits xs) (floor ( (sumOfMarks xs) / (sumOfCredits xs) ) ) ]

minimazi :: [[ModuleResult]] -> [[ModuleResult]]
minimazi [] = []
minimazi (x:xs) = calculateYearAverage x : minimazi xs

creditsInFirst :: [ModuleResult] -> Float
creditsInFirst [] = 0.0
creditsInFirst ((ModuleResult c m):xs)
  | m >= 70 = c + creditsInFirst xs
  | otherwise = creditsInFirst xs

creditsInUpperSecond :: [ModuleResult] -> Float
creditsInUpperSecond [] = 0.0
creditsInUpperSecond ((ModuleResult c m):xs)
  | m >= 60 = c + creditsInUpperSecond xs
  | otherwise = creditsInUpperSecond xs

creditsInLowerSecond :: [ModuleResult] -> Float
creditsInLowerSecond [] = 0.0
creditsInLowerSecond ((ModuleResult c m):xs)
  | m >= 50 = c + creditsInLowerSecond xs
  | otherwise = creditsInLowerSecond xs


clasifyMark :: [[ModuleResult]] -> Float -> DegreeClass
clasifyMark ms x
  | x>=68 && x<70 && ((creditsInFirst (concat ms)) * 100) / (sumOfCredits (concat ms)) >= 50.0  = First
  | x>=58 && x<60 && ((creditsInUpperSecond (concat ms)) * 100) / (sumOfCredits (concat ms)) >= 50.0  = UpperSecond
  | x>=48 && x<50 && ((creditsInLowerSecond (concat ms)) * 100) / (sumOfCredits (concat ms)) >= 50.0  = LowerSecond
  | floor x >=70 = First
  | floor x>=60 && floor x < 70 = UpperSecond
  | floor x>=50 && floor x < 60 = LowerSecond
  | floor x>=40 && floor x < 50 = Third


getMark :: ModuleResult -> Float
getMark (ModuleResult c m) = fromIntegral m

classify :: [[ModuleResult]] -> DegreeClass
classify ms
  | length ms == 3 = clasifyMark (tail ms) ( (getMark s) * 1/3 + (getMark t) * 2/3 )
  | length ms == 4 = clasifyMark (tail ms) ( (getMark s) * 1/5 + (getMark t) * 2/5 + (getMark l) * 2/5 )
    where
      s = head $ head $ tail $ minimazi ms
      t = head $ head $ tail $ tail $ minimazi ms
      l = head $ last $ minimazi ms

-- Exercise 5
-- search for the local maximum of f between x and x' using an
-- approximation margin eps
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps
  | abs (x2 - x3) <= eps = middle
  | d x3 > d x2 = hillClimb d x2 x' eps
  | otherwise = hillClimb d x x3 eps
  where
    middle = (x' + x) / 2
    sigma = (1 + sqrt(5))/2
    x2 = x' - (x' - x) / sigma
    x3 = x + (x' - x) / sigma

-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = hillClimb1 (\x -> createPolinom xs x) x x' eps

hillClimb1 :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb1 d x x' eps
  | abs (x2 - x3) <= eps = middle
  | (d x2)^2 < (d x3)^2 = hillClimb1 d x x3 eps
  | otherwise = hillClimb1 d x2 x' eps
  where
    middle = (x' + x) / 2
    sigma = (1 + sqrt(5))/2
    x2 = x' - (x' - x) / sigma
    x3 = x + (x' - x) / sigma

createPolinom :: [Float] -> Float -> Float
createPolinom [] _ = 0
createPolinom xs x = last xs * ( x ^ (length xs - 1)) + result
  where
    result =  createPolinom (init xs) x

-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence [] ins = []
executeInstructionSequence ns [] = ns
executeInstructionSequence ns ins
  | head ins == Add && length ns > 1 = executeInstructionSequence (replace ns addition) (tail ins)
  -- | head ins == Subtract && length ns > 1 = executeInstructionSequence (replace ns substract) (tail ins)
  | head ins == Multiply && length ns > 1 = executeInstructionSequence (replace ns multiply) (tail ins)
  | head ins == Duplicate = executeInstructionSequence ([head ns] ++ ns) (tail ins)
  | head ins == Pop = executeInstructionSequence (tail ns) (tail ins)
  where
    addition = head ns + (head (tail ns))
  --  substract = head ns - (head (tail ns))
    multiply = head ns * (head (tail ns))

replace :: [a] -> a -> [a]
replace xs y = [y] ++ tail( tail xs)

-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence 0 = []
optimalSequence n
  | powerOfTwo n == True =  powerOfTwoSequence (whatPower n 0)
  | mod n 2 == 1 = [Duplicate] ++ optimalSequence (n-1) ++ [Multiply]
  | mod n 2 == 0 =  optimalSequence (div n 2) ++ [Duplicate,Multiply]
  | otherwise = []

powerOfTwoSequence :: Int -> [Instruction]
powerOfTwoSequence n
  | n > 0 = [Duplicate, Multiply] ++ powerOfTwoSequence (n-1)
  | otherwise = []

whatPower :: Int -> Int -> Int
whatPower x y
  | powerOfTwo x /= True = -1
  | 2 ^ y == x = y
  | otherwise = whatPower x (y+1)

powerOfTwo :: Int -> Bool
powerOfTwo 0 = False
powerOfTwo x
  | x /= 1 && (divided == 0) = powerOfTwo ( round ( (fromIntegral x) /2) )
  | x == 1 = True
  | otherwise = False
    where
      divided = mod (fromIntegral x) 2

-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers [] = []
findBusyBeavers [x] = [[]]
findBusyBeavers ns
  | length ns >= 2 && head ns == 0  && (head (tail ns)) == 0 = (map (Add :) (findBusyBeavers newStack1)) ++ (map (Pop :) (findBusyBeavers ( tail ns)) ) ++ (map(Multiply :) (findBusyBeavers newStack))
  | checkForNegatives ns == False = createListOfEqualSolutions ns (findAllSolutionsForNegatives ns) (findAllSolutionsForNegatives ns)
  | head ns == 0 = (map (Add :) (findBusyBeavers newStack1)) ++ (map (Pop :) (findBusyBeavers ( tail ns)) )
  | head ns + head (tail ns) == head ns * head (tail ns) = (map (Add :) (findBusyBeavers newStack1)) ++ (map(Multiply :) (findBusyBeavers newStack))
  | head ns + head (tail ns) < head ns * head (tail ns) = map (Multiply :) (findBusyBeavers newStack)
  | otherwise = map (Add :) (findBusyBeavers newStack1)
 where
    newStack = executeInstructionSequence ns [Multiply]
    newStack1 = executeInstructionSequence ns [Add]

checkForNegatives :: [Int] -> Bool
checkForNegatives [] = True
checkForNegatives (x:xs)
  | x < 0 = False && checkForNegatives xs
  |otherwise = checkForNegatives xs

findAllSolutionsForNegatives :: [Int] -> [[Instruction]]
findAllSolutionsForNegatives [] = []
findAllSolutionsForNegatives [x] = [[]]
findAllSolutionsForNegatives ns = (map (Add :) (findAllSolutionsForNegatives newStack1)) ++ (map (Pop :) (findAllSolutionsForNegatives ( tail ns)) ) ++ map (Multiply :) (findAllSolutionsForNegatives newStack)
  where
     newStack = executeInstructionSequence ns [Multiply]
     newStack1 = executeInstructionSequence ns [Add]

compareTwoSolutions :: [Int] -> [Instruction] -> [Instruction] -> [Instruction]
compareTwoSolutions ns x y
  | head (executeInstructionSequence ns x) >=  head (executeInstructionSequence ns y) = x
  | otherwise = y

findBestSolutionForNegatives :: [Int] -> [[Instruction]] -> [Instruction]
findBestSolutionForNegatives _ [] = []
findBestSolutionForNegatives _ ([x]:[]) = [x]
findBestSolutionForNegatives ns (x:xs) = foldr ( compareTwoSolutions ns ) x xs

createListOfEqualSolutions :: [Int] -> [[Instruction]] -> [[Instruction]] -> [[Instruction]]
createListOfEqualSolutions _ _ [] = []
createListOfEqualSolutions ns es (x:xs)
  | head (executeInstructionSequence ns x) ==  head (executeInstructionSequence ns bestSolution) = x : createListOfEqualSolutions ns es xs
  | otherwise = createListOfEqualSolutions ns es xs
    where
      bestSolution = findBestSolutionForNegatives ns es

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList [] = []
simplifyRectangleList rs = removeDuplicates (simplifyWrapper rs rs)

eliminateIncludedRectangle :: [Rectangle] -> Rectangle -> [Rectangle]
eliminateIncludedRectangle [] _ = []
eliminateIncludedRectangle ((Rectangle (x1, y1)  (x2, y2)):rs) (Rectangle (x1', y1')  (x2', y2'))
  | x2 < x1 || y2 < y1
   || (x2 < x2' && y2 < y2' && x1 > x1' && y1 > y1')
   || (x2 < x2' && y2 < y2' && x1 == x1' && y1 == y1')
   || (x2 < x2' && y2 == y2' && x1 == x1' && y1 > y1')
   || (x2 == x2' && y2 == y2' && x1 > x1' && y1 > y1')
   || (x2 == x2' && y2 < y2' && x1 > x1' && y1 == y1')
   || (x2 == x2' && y2 < y2' && x1 == x1' && y1 == y1')
   || (x2 == x2' && y2 == y2' && x1 == x1' && y1 > y1')
   || (x2 == x2' && y2 < y2' && x1 == x1' && y1 > y1')
   || (x2 < x2' && y2 < y2' && x1 == x1' && y1 > y1') --
   || (x2 < x2' && y2 == y2' && x1 > x1' && y1 > y1')
   || (x2 == x2' && y2 < y2' && x1 > x1' && y1 > y1')
   || (x2 < x2' && y2 < y2' && x1 > x1' && y1 == y1')
   || (x2 < x2' && y2 == y2' && x1 > x1' && y1 == y1')
   || (x2 < x2' && y2 == y2' && x1 == x1' && y1 == y1')
   || (x2 == x2' && y2 == y2' && x1 > x1' && y1 == y1') = eliminateIncludedRectangle rs (Rectangle (x1', y1') (x2', y2'))
  -- | (x2 > x2' && y2 == y2' && x1 == x2'  && y1 == y1') = [y] ++ eliminateIncludedRectangle rs (Rectangle (x1', y1')  (x2', y2'))
  | otherwise = [x] ++ eliminateIncludedRectangle rs (Rectangle (x1', y1')  (x2', y2'))
    where
      x = Rectangle (x1, y1)  (x2, y2)
  --    y = Rectangle (x1', y1')  (x2, y2)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/=x) xs)

simplifyWrapper :: [Rectangle] -> [Rectangle] -> [Rectangle]
simplifyWrapper [] ys = ys
simplifyWrapper (x:xs) ys  = simplifyWrapper xs ( eliminateIncludedRectangle ys x)

-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b
  | a >= 1 && b >= 1 = simplifyRectangleList ((createRectanglesY x y a b (truncate (y + b)) (truncate (x + a)) (truncate (y - b)) (truncate (x - a)))
  ++ (createRectanglesForX x y a b (truncate (y + b)) (truncate (x + a)) (truncate (y - b)) (truncate (x - a))))
  | otherwise = [Rectangle (0,0) (0,0)]

createRectanglesForY :: Float -> Float -> Float -> Float -> Int -> Int -> Int -> Int -> [Rectangle]
createRectanglesForY x y a b bondYpoz bondXpoz bondYneg bondXneg
  | bondXpoz < floor x = []
  | result <=1 && result2 <=1 = [ Rectangle (bondXneg,bondYneg) (bondXpoz,bondYpoz)] 
  | otherwise = createRectanglesForY x y a b bondYpoz (bondXpoz - 1) bondYneg (bondXneg + 1)
    where
      result = (( (fromIntegral bondXpoz) - x)) ^ 2 / a^2 + (( (fromIntegral bondYpoz) - y)) ^ 2 / b^2
      result2 = (( (fromIntegral bondXneg) - x)) ^ 2 / a^2 + (( (fromIntegral bondYneg) - y)) ^ 2 / b^2

createRectanglesForX :: Float -> Float -> Float -> Float -> Int -> Int -> Int -> Int -> [Rectangle]
createRectanglesForX x y a b bondYpoz bondXpoz bondYneg bondXneg
  | bondYpoz < floor y = []
  | result <=1 && result2 <= 1 = [ Rectangle (bondXneg,bondYneg) (bondXpoz,bondYpoz)]
  | otherwise = createRectanglesForX x y a b (bondYpoz - 1) bondXpoz  (bondYneg + 1) bondXneg
    where
      result = (( (fromIntegral bondXpoz) - x)) ^ 2 / a^2 + (( (fromIntegral bondYpoz) - y)) ^ 2 / b^2
      result2 = (( (fromIntegral bondXneg) - x)) ^ 2 / a^2 + (( (fromIntegral bondYneg) - y)) ^ 2 / b^2

createRectanglesY :: Float -> Float -> Float -> Float -> Int -> Int -> Int -> Int -> [Rectangle]
createRectanglesY x y a b bondYpoz bondXpoz bondYneg bondXneg
  | bondYpoz <= floor y = []
  | otherwise = createRectanglesForY x y a b bondYpoz bondXpoz bondYneg bondXneg  ++ createRectanglesY x y a b (bondYpoz-1) bondXpoz (bondYneg + 1) bondXneg


-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage s = integerToString ( stringToInteger s )

stringToInteger :: String -> [Int]
stringToInteger [] = []
stringToInteger (x:xs)
  | x == '1' = [1] ++ stringToInteger xs
  | x == '0' = [0] ++ stringToInteger xs
  | otherwise = stringToInteger xs

integerToString :: [Int] -> String
integerToString [] = ""
integerToString (x:[]) = ""
integerToString (x:y:xs)
  | x == 0 && y == 0 = ['a'] ++ integerToString xs
  | x == 0 && y == 1 = ['b'] ++ integerToString xs
  | x == 1 && y == 0 = ['c'] ++ integerToString xs
  | x == 1 && y == 1 = ['d'] ++ integerToString xs

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method
differentStream :: [[Int]] -> [Int]
differentStream ss = choose ss 0

choose :: [[Int]] -> Int ->[Int]
choose (x:ss) nr
  | (x !! nr) /= 0 = 0 : choose ss (nr + 1)
  | (x !! nr) /= 1 = 1 : choose ss (nr + 1)

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f
  | n - m*m < m = f (n-m*m) m
  | otherwise = f m (m*m + 2*m - n )
    where
      m = floor(  sqrt( fromIntegral n))

-- Exercise 15
isShellTreeSum :: Int -> Bool
isShellTreeSum n
   | findSumOfTree first == second = True
   | otherwise = False
    where
      first = unPairFirst n
      second = unPairSecond n

unPairFirst :: Int -> Int
unPairFirst n
  | n - m*m < m = (n-m*m)
  | otherwise = m
    where
      m = floor(  sqrt( fromIntegral n))

unPairSecond :: Int -> Int
unPairSecond n
  | n - m*m < m = m
  | otherwise = (m*m + 2*m - n )
      where
        m = floor(  sqrt( fromIntegral n))

findSumOfTree :: Int -> Int
findSumOfTree 0 = 0
findSumOfTree 1 = 0
findSumOfTree x = unPairFirst x +  findSumOfTree (unPairFirst (unPairSecond x) )  + findSumOfTree (unPairSecond (unPairSecond x) )
