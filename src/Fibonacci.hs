module Fibonacci (fibWord) where

import qualified Data.Vector.Unboxed as V

fibWord :: Int -> V.Vector Bool
fibWord n = fibWord' n (V.fromList [False]) (V.fromList [False,True])
    where
        fibWord' :: Int ->  V.Vector Bool -> V.Vector Bool -> V.Vector Bool
        fibWord' 0 v0 _ = v0
        fibWord' n v0 v1 = let v' = v1 V.++ v0 in
                           fibWord' (pred n) v1 v'

-- |
-- >>> putStrLn . concat . map (show . fromEnum) . V.toList $ fibWord 4
-- 01001010
-- >>> putStrLn . concat . map (show . fromEnum) . V.toList $ fibWord 5
-- 0100101001001
