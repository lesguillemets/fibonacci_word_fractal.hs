import Fibonacci
import Draw

import System.Environment (getArgs)
import Text.Read (readMaybe)

main = do
    l <- getArgs
    drawFib . fibWord $ case map readMaybe l of
                             (Just d:_) -> d
                             _ -> 16
