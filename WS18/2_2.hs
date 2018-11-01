div' :: Int -> Int -> Maybe Int
div' _ 0 = Nothing
div' x y = Just (x `div` y)
