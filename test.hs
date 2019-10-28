rep':: (Eq a, Num a) => a -> b -> [b]
rep' 0 e = []
rep' 1 e = [e]
rep' n e = e : (rep' ((abs n) - 1) e)


-- Take function 
take' :: (Num i , Ord i) => i -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' 1 (x:xs) = [x]
take' n (x:xs)
  | n < 0 = []
  | otherwise = x : (take' (n - 1) xs)


-- Reverse function 
rev [] = []
rev (x:xs) = (rev xs) ++ [x]