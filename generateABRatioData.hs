-- Generate curves with constant a-b ratio

import Solve
import KP
import Data.List (zip7,unzip5,intersperse)
import Control.Applicative ((<*>))

---------- Root finding ----------
primaryIntervals func = 
  [search func start 1e-2  | (_, start) <- (0,0):primaryIntervals func]

primaryRoots func =  map (ridder func) (primaryIntervals func)

secondaryIntervals func =
  first:concat [[(x1, ext x1 x2), (ext x1 x2, x2)] | (x1, x2) <- intervals]
    where
      intervals = zip (primaryRoots func) $ tail $ primaryRoots func
      ext x1 x2 = findExtreme func (x1, x2)
      first = (0, fst (head intervals))

secondaryRoots func =
  map (solve func) $ secondaryIntervals func
  where
    solve func int@(x1, x2) = ridder modFunc int
      where
        modFunc x = func x - offset
        offset = signum $ func $ (x2 + x1) / 2

findExtreme func (start, stop) =
  bisection (numDeriv func) (start, stop)

allowedBands func = group $ secondaryRoots func
forbiddenRegions func = group $ tail $ secondaryRoots func

allowedWidths func = [b - a | (a,b) <- allowedBands func]
forbiddenWidths func = [b - a | (a,b) <- forbiddenRegions func]

nthBandEnergy n func = allowedWidths func !! (n - 1)
nthGapEnergy n func = forbiddenWidths func !! (n - 1)

---------- Other Functions ----------
group :: [a] -> [(a, a)]
group [] = []
group (_:[]) = []
group (a:b:[]) = [(a,b)]
group (a:b:xs) = (a,b):group xs

cartProd a b            = [(a1,b1) | a1 <- a, b1 <- b]
cartProd3 a b c         = [(a1,b1,c1) | a1 <- a, b1 <- b, c1 <- c]
cartProd4 a b c d       = [(a1,b1,c1,d1) | a1 <- a, b1 <- b, c1 <- c, d1 <- d]
cartProd5 a b c d e     = [(a1,b1,c1,d1,e1) | a1 <- a, b1 <- b, c1 <- c, d1 <- d, e1 <- e]

uncurry3 f (a, b, c)       = f a b c
uncurry4 f (a, b, c, d)    = f a b c d
uncurry5 f (a, b, c, d, e) = f a b c d e

csvify2 (a,b)           = show a ++ ", " ++ show b
csvify3 (a,b,c)         = show a ++ ", " ++ show b ++ ", " ++ show c
csvify4 (a,b,c,d)       = show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d
csvify5 (a,b,c,d,e)     = show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ", " ++ show e
csvify6 (a,b,c,d,e,f)   = show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ", " ++ show e ++ ", " ++ show f
--csvify7 (a,b,c,d,e,f,g) = show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ", " ++ show e ++ ", " ++ show f ++ ", " ++ show g
csvify7 (a,b,c,d,e,f,g) = concat $ intersperse ", " $ map show items
  where items = [a,b,c,d,e,f,g]

---------- Data Generation Related Functions ----------
-- Return columns: a, b, v_0, p, ratio
getParams :: [Double] -> Double -> [(Double, Double, Double, Double, Double)]
getParams xs ratio =
  concat [[(a p v_0, b (a p v_0), v_0, p, ratio) | p <- xs] | v_0 <- v0s]
    where
      v0s = logspace 0.5 5 11
      a p v_0 = hbar * sqrt ((p * ratio) / (m * v_0))
      b a = a / ratio

---------- Do actual data generation ----------
sPowers = logspace 0.1 1000 31
ratios = logspace 0.05 20 6

-- rawInputs: [(a, b, v_0, p, ratio)]
rawInputs = concat $ map (getParams sPowers) ratios

inputs = filter pred rawInputs
  where
    pred (a,b,v,_,_) = if charEqu a b v 0 > 2.0 && charEqu a b v 0 < 1e10
                            then True
                            else False

equs = [charEqu a b v | (a,b,v,_,_) <- inputs]
simpEqus = [simpCharEqu a b v | (a,b,v,_,_) <- inputs]

firstEnergy = map (nthGapEnergy 1) equs
simpFirstEnergy = map (nthGapEnergy 1) simpEqus

-- Display data - csv style
displayData ins normEnergy normSimpEnergy = do
  putStrLn "a, b, ratio, v_0, p, norm energy, norm simp energy"
  mapM_ putStrLn $ map csvify7 $ zip7 a b r v p normEnergy normSimpEnergy
    where
      (a,b,v,p,r) = unzip5 ins

main = displayData inputs normEnergy normSimpEnergy
  where
    normSimpEnergy = map (uncurry normalize) $ zip a simpFirstEnergy
    normEnergy = map (uncurry normalize) $ zip a firstEnergy
    (a,_,_,_,_) = unzip5 inputs
