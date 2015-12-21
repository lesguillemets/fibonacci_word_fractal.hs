{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Draw (drawFib, drawFibRainbow) where

import qualified Data.Vector.Unboxed as V
import Control.Arrow
import GHC.Float (double2Float)
import Graphics.Gloss
import Data.List

import Fibonacci
import Colour

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

drawFibWithColour :: (Int -> Color) -> V.Vector Bool -> IO ()
drawFibWithColour f = display (window "fib!") backColour . toPict f

drawFib :: V.Vector Bool -> IO ()
drawFib = drawFibWithColour (const lineColour)

drawFibRainbow :: V.Vector Bool -> IO ()
drawFibRainbow ds = drawFibWithColour f ds
    where
        l = fromIntegral $ V.length ds
        f i = fromHSV (fromIntegral i*360/l, 1,1)

toPict :: (Int -> Color) -- Index to Colour
        -> V.Vector Bool -> Picture
toPict f = follow f . map toTurn . zip (cycle [True,False]) . V.toList
    
follow :: (Int -> Color) -> [Int] -> Picture
follow colourFun turns = Pictures $ f initTurtle (zip [0..] turns)
    where
        f :: Turtle -> [(Int,Int)] -> [Picture]
        f _ [] = []
        f !(t@Turtle{..}) ((colN,d):xs) =
            let newLoc = _loc +: forward t basicLength
                newAngle = _dir + basicAngle * fromIntegral d
                currentEdge = path (colourFun colN) _loc newLoc
                in
                currentEdge : f (Turtle newLoc newAngle) xs

path :: Color -> (Double, Double) -> (Double, Double) -> Picture
path col p0 p1 = color col . line . map floatise $ [p0, p1]

data Turtle = Turtle {_loc :: (Double, Double), _dir :: Double}
initTurtle :: Turtle
initTurtle = Turtle (0,0) 0
forward :: Turtle -> Double -> (Double,Double)
forward t d = ((*d) . cos &&& (*d) . sin) . _dir $ t
floatise :: (Double,Double) -> (Float,Float)
floatise (x,y) = (double2Float x, double2Float y)
(+:) :: (Double,Double) -> (Double,Double) -> (Double,Double)
(a,b) +: (c,d) = (a+c,b+d)
