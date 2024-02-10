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
