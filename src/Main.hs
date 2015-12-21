{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -O3 -threaded #-}
import Fibonacci
import Draw

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Options.Declarative
import Control.Monad.IO.Class (liftIO)

draw ::
    Flag "n" '["level"] "Int" "draws fib n" (Def "16" Int)
 -> Flag "a" '["angle"] "Double" "Angle (/pi) to Turn." (Def "0.5" Double)
 -> Flag "r" '["raw-radian"] "Bool"
     "When given, the angle is treated as raw value in radians. When not, '-a x' is treated as pi*x radian."
     Bool
 -> Flag "c" '["with-color"] "Bool" "Use color?" Bool
 -> Cmd "Draw Fibonacci Word Fractal" ()

draw n a r c =
    let θ = if get r then get a else get a*pi
        f = if get c then drawFibRainbow
                     else drawFibMonochrome
        in
        liftIO . f θ . fibWord $ get n

main :: IO()
main = run_ draw
