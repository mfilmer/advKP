module Newton (step, solve, slope, guess) where
import Data.List
import Data.Maybe

-- Made up number for epsilon, probably shouldn't call it epsilon... that's confusing
epsilon = 1e-5
slope :: (Double -> Double) -> Double -> Double
slope func x = (func (x+inc) - func (x-inc))/(2*inc)
  where
    inc = if x == 0
            then epsilon
            else x * epsilon

step :: (Double -> Double) -> Double -> Double
step func x = x - (func x) / (slope func x)

solve :: (Double -> Double) -> Double -> Double
solve func guess = iterate (step func) guess !! 35

guess :: (Double -> Double) -> (Double, Double) -> Double
guess func (start, stop) = xValues !! (fromJust $ elemIndex (minimum funcVals) funcVals)
  where
    step = (stop - start) / 101
    xValues = [start, start + step .. stop]
    funcVals = map (abs.func) xValues