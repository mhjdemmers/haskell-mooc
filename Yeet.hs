
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
