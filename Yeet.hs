import qualified Data.Char as Char
import Distribution.Simple.Utils (xargs)

maxBy :: (a -> Int) -> a -> a -> a
maxBy measure a b
  | measure a >= measure b = a
  | otherwise              = b

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

mapMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mapMaybe2 f Nothing y = Nothing
mapMaybe2 f x Nothing = Nothing
mapMaybe2 f (Just x) (Just y) = Just ( f x y)

palindromeHalfs :: [String] -> [String]
palindromeHalfs xs = map firstHalf (filter palindrome xs)

firstHalf :: String -> String
firstHalf str = take (div (length str + 1) 2) str

palindrome str = str == reverse str

capitalize :: String -> String
capitalize str = unwords (map capitalizeFirst (words str))

capitalizeFirst :: String -> String
capitalizeFirst (head:tail) = Char.toUpper head : tail

powers :: Int -> Int -> [Int]
powers k max = takeWhile (<= max) [k^i | i <- [0..]]

whileRight :: (a -> Either b a) -> a -> b
whileRight check x = case check x of Left x -> x
                                     Right y -> whileRight check y

step :: Int -> Int -> Either Int Int
step k x = if x<k then Right (2*x) else Left x

bomb :: Int -> Either String Int
bomb 0 = Left "BOOM"
bomb x = Right (x-1) 


(+|+) :: [a] -> [a] -> [a]
a +|+ [] = [head a]
[] +|+ b = [head b]
a +|+ b = [head a, head b]