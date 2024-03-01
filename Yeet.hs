import Data.List
import Control.Applicative (Alternative(some))

applyTo1 :: (Int -> Int) -> Int 
applyTo1 f = f 1

addThree :: Int -> Int
addThree x = x + 3

-- doTwice :: (a -> a) -> a -> a
-- doTwice f x = f (f x)

makeCool :: String -> String
makeCool str = "WOW " ++ str ++ "!"

positive :: Int -> Bool
positive x = x > 0

onlyPositive xs = filter positive xs

mapBooleans f = map f [False,True]

wrapJust xs = map Just xs

-- a predicate that checks if a string is a palindrome
palindrome :: String -> Bool
palindrome str = str == reverse str

-- palindromes n takes all nembers from 1 to n, converts them to strings using show, and keeps only palindromes
palindromes :: Int -> [String]
palindromes n = filter palindrome (map show [1..n])

countAWords :: String -> Int
countAWords string = length (filter startsWithA (words string))
  where startsWithA s = head s == 'a'

-- substringsOfLength :: Int -> String -> [String]
-- substringsOfLength n string = map shorten (tails string)
--   where shorten s = take n s

-- whatFollows :: Char -> Int -> String -> [String]
-- whatFollows c k string = map tail (filter match (substringsOfLength (k+1) string))
--   where match sub = take 1 sub == [c]

between :: Integer -> Integer -> Integer -> Bool
between lo high x = x < high && x > lo

doTwice :: (a -> a) -> a -> a
doTwice f = f . f

whatFollows :: Char -> Int -> String -> [String]
whatFollows c k = map tail . filter ((==[c]) . take 1) . map (take (k+1)) . tails

findSubstring :: String -> String -> String
findSubstring chars = takeWhile (\x -> elem x chars)
                      . dropWhile (\x -> not $ elem x chars)

split :: Char -> String -> [String]
split c [] = []
split c xs = start : split c (drop 1 rest)
  where start = takeWhile (/=c) xs
        rest = dropWhile (/=c) xs

myhead :: [Int] -> Int
myhead [] = -1
myhead (first:rest) = first

mytail :: [Int] -> [Int]
mytail [] = []
mytail (first:rest) = rest

sumFirstTwo :: [Integer] -> Integer
-- this equation gets used for lists of length at least two
sumFirstTwo (a:b:_) = a + b
-- this equation gets used for all other lists (i.e. lists of length 0 or 1)
sumFirstTwo _ = 0

describeList :: [Int] -> String
describeList [] = "an empty list"
describeList [x] = "a list with one element"
describeList [x,y] = "a list with two elements"
describeList (x:y:z:xs) = "a list with at least three elements"

startsWithZero :: [Integer] -> Bool
startsWithZero (0:xs) = True
startsWithZero (x:xs) = False
startsWithZero [] = False

-- Not tail recursive!
-- sumNumbers :: [Int] -> Int
-- sumNumbers [] = 0 
-- sumNumbers (x:xs) = x + sumNumbers xs

-- Tail recursive version
sumNumbers :: [Int] -> Int
sumNumbers xs = go 0 xs
  where go sum [] = sum
        go sum (x:xs) = go (sum+x) xs

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = go x xs
  where go biggest [] = biggest
        go biggest (x:xs) = go (max biggest x) xs

countNothings :: [Maybe a] -> Int
countNothings [] = 0 
countNothings (Nothing : xs) = 1 + countNothings xs
countNothings (Just _ : xs) = countNothings xs

-- Not tail recursive!
-- doableList :: [Int] -> [Int]
-- doubleList [] = []
-- doubleList (x:xs) = 2*x : doubleList xs

-- Tail recursive version
doubleList :: [Int] -> [Int]
doubleList xs = go [] xs
  where go result [] = result
        go result (x:xs) = go (result++[2*x]) xs

(<+>) :: [Int] -> [Int] -> [Int]
xs <+> ys = zipWith (+) xs ys

(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b

keepElements :: [a] -> [Bool] -> [a]
keepElements xs bs = map fst (filter snd (zip xs bs))
