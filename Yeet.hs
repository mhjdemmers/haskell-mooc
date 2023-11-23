readInt :: String -> Either String Int
readInt "0" = Right 0
readInt "1" = Right 1
readInt s = Left ("Unsuppoerted string: " ++ s)

iWantAString :: Either Int String -> String
iWantAString (Right str)   = str
iWantAString (Left number) = show number
