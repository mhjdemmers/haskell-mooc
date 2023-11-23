direction :: Either Int Int -> String
direction (Left i) = "you should go left " ++ show i ++ " meters!"
direction (Right i) = "you should go right " ++ show i ++ " meters!"