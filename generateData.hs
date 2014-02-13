import Newton
import KP

equ = charEqu 5e-10 1e-10 (evToJ 1.4)

main = mapM_ putStrLn [show x ++ ", " ++ show (equ x) | x <- [0.01, 0.02 .. 50]]

-- getBand takes a function and a starting point
-- and returns the start and end points of the
-- first allowed band right of the start point
getBand :: (Double -> Double) -> Double -> (Double, Double)
getBand func start = (start, stop)
  where
    start = solve func (approxNextZero func start)
    stop = 

approxNextZero :: (Double -> Double) -> Double -> Double
approxNextZero func start =
  fst $ head $ dropWhile (\(_, x) -> signum x == firstSign) funcVals
    where
      firstSign = signum $ head funcVals
      funcVals = zip xs (map func xs)
        where
          xs = [start, start + 0.0001 ..]