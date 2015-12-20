{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Draw (drawFib) where

import qualified Data.Vector.Unboxed as V
import Control.Arrow
import GHC.Float (double2Float)
import Graphics.Gloss
import Data.List

import Fibonacci

lineColour :: Color
lineColour = white
backColour :: Color
backColour = black
basicLength :: Double
basicLength = 5
basicAngle :: Double
basicAngle = pi/2

window :: String -> Display
window s = InWindow s (500,500) (30,30)

toTurn :: (Bool, Bool) -> Int
toTurn (locEven,b) = if b
                         then (if locEven then 1 else (-1 ))
                         else 0

drawFib :: V.Vector Bool -> IO ()
drawFib = display (window "fib!") backColour . toPict

toPict :: V.Vector Bool -> Picture
toPict = follow . map toTurn . zip (cycle [True,False]) . V.toList
    
follow :: [Int] -> Picture
follow turns = Pictures $ f initTurtle turns
    where
        f :: Turtle -> [Int] -> [Picture]
        f _ [] = []
        f !(t@Turtle{..}) (d:ds) =
            let newLoc = _loc +: forward t basicLength
                newAngle = _dir + basicAngle * fromIntegral d
                currentEdge = path _loc newLoc
                in
                currentEdge : f (Turtle newLoc newAngle) ds

path :: (Double, Double) -> (Double, Double) -> Picture
path p0 p1 = color lineColour . line . map floatise $ [p0, p1]

data Turtle = Turtle {_loc :: (Double, Double), _dir :: Double}
initTurtle :: Turtle
initTurtle = Turtle (0,0) 0
forward :: Turtle -> Double -> (Double,Double)
forward t d = ((*d) . cos &&& (*d) . sin) . _dir $ t
floatise :: (Double,Double) -> (Float,Float)
floatise (x,y) = (double2Float x, double2Float y)
(+:) :: (Double,Double) -> (Double,Double) -> (Double,Double)
(a,b) +: (c,d) = (a+c,b+d)
