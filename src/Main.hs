{-# OPTIONS_GHC -O3 -threaded #-}
import Fibonacci
import Draw

import System.Environment (getArgs)
import Text.Read (readMaybe)

main = do
    l <- getArgs
    drawFibRainbow (3*pi/7) . fibWord $ case map readMaybe l of
                             (Just d:_) -> d
                             _ -> 16
