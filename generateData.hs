import Newton
import KP

equ = charEqu 5e-10 1e-10 (evToJ 1.4)

main = mapM_ putStrLn [show x ++ ", " ++ show (equ x) | x <- [0.01, 0.02 .. 50]]

-- getEdge takes a function and a starting point
-- and returns the next band edge
getEdge :: (Double -> Double) -> Double -> Double
getEdge func start = edge
  where
    edge = solve func (approxNextEdge func start)

approxNextEdge :: (Double -> Double) -> Double -> Double
approxNextEdge func start =
  fst $ head $ dropWhile (\(_, x) -> signum (x - edgeSign) == firstSign) funcVals
    where
      edgeSign
        | startSlope == 1 && startVal > 0 = 1
        | startSlope == 1 && startVal < 0 = (-1)
        | startSlope == -1 && startVal < 0 = (-1)
        | startSlope == -1 && startVal > 0 = 1
          where
            startVal = func start
            startSlope = signum $ slope func start
      firstSign = signum $ snd (head funcVals) - edgeSign
      funcVals = zip xs (map func xs)
        where
          xs = [start, start + 0.0001 ..]