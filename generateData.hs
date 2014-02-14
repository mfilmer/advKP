import Newton
import KP


-- First band gap energy for a variety of values of a
-- aVals = logspace 5e-10 1e-8 11
aVals = linspace 1e-10 1e-9 10
aEqus = map (\a x -> charEqu a 1e-10 1.4 x) aVals
getFirstGap equ = diffs (tail edges) !! 0
  where
    edges = [getEdge equ (x + 0.000001) | x <- 0:edges]
firstGaps = map getFirstGap aEqus
-- main = mapM_ putStrLn [show val ++ ", " ++ show equ | (val,equ) <- zip aVals firstGaps]

-- Band energies for the nth band
simpleEqu = charEqu 1e-10 1e-10 1.4
bandEdges = [getEdge simpleEqu (x + 0.000001) | x <- 0:bandEdges]
bandGaps = diffs (tail bandEdges)
allowedBands = diffs bandEdges
main = mapM_ putStrLn [show x ++ ", " ++ show (simpleEqu x) | x <- [0.01, 0.02 .. 500]]
-- main = mapM_ putStrLn [show x ++ ", " ++ show (bandGaps !! x) ++ ", " ++ show (allowedBands !! x)| x <- [0, 1 .. 20]]

-- Calculate difference between groups of 2 elements in a list
diffs :: (Num a) => [a] -> [a]
diffs (a:b:xs) = (b - a):(diffs xs)

-- getEdge takes a function and a starting point
-- and returns the next band edge
getEdge :: (Double -> Double) -> Double -> Double
getEdge func start = edge
  where
    edge = solve (\x -> simpleEqu x - (signum (func x))) (approxNextEdge func start)

approxNextEdge :: (Double -> Double) -> Double -> Double
approxNextEdge func start =
  fst $ head $ dropWhile (\(_, x) -> signum (x - edgeSign) == firstSign) funcVals
    where
      edgeSign
        | startSlope == 1 && startVal >= -1 = 1
        | startSlope == 1 && startVal < -1 = -1
        | startSlope == -1 && startVal <= 1 = -1
        | startSlope == -1 && startVal > 1 = 1
        | startSlope == 0 && startVal > 1 = 1
        | startSlope == 0 && startVal < 1 = -1
          where
            startVal = func start
            startSlope = round $ signum $ slope func start
      firstSign = signum $ snd (head funcVals) - edgeSign
      funcVals = zip xs (map func xs)
        where
          xs = [start, start + 0.0001 ..]