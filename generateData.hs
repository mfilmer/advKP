import Solve
import KP
import Data.List (zip6)

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

csvify2 (a,b)         = show a ++ ", " ++ show b
csvify3 (a,b,c)       = show a ++ ", " ++ show b ++ ", " ++ show c
csvify4 (a,b,c,d)     = show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d
csvify5 (a,b,c,d,e)   = show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ", " ++ show e
csvify6 (a,b,c,d,e,f) = show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ", " ++ show e ++ ", " ++ show f

---------- Do actual data generation ----------
--sampEqu = charEqu 1e-10 5e-10 3
--sampEqu = charEqu 1e-11 1e-11 0.5
sampEqu = (uncurry3 charEqu) (inputs !! 102)

allowedBands func = group $ secondaryRoots func
forbiddenRegions func = group $ tail $ secondaryRoots func

allowedWidths func = [b - a | (a,b) <- allowedBands func]
forbiddenWidths func = [b - a | (a,b) <- forbiddenRegions func]

nthBandEnergy n func = allowedWidths func !! (n - 1)
nthGapEnergy n func = forbiddenWidths func !! (n - 1)

-- Vary everything and calculate everything
a' = linspace 1e-10 5e-9 11    -- [m]
b' = linspace 1e-10 5e-9 11    -- [m]
v_0' = linspace 0.5 5 5       -- [eV]

inputs = filter pred inputs'
  where
    inputs' = cartProd3 a' b' v_0'
    pred (a,b,v) = if charEqu a b v 0 > 2.0
                    then True
                    else False
ps = map (uncurry3 scatteringPower) inputs

equs = map (uncurry3 charEqu) inputs
simpEqus = map (uncurry3 simpCharEqu) inputs

firstEnergy = map (nthGapEnergy 1) equs
simpFirstEnergy = map (nthGapEnergy 1) simpEqus

-- Display csv style
displayData ins p simpEnergy normSimpEnergy = do
  putStrLn "a, b, v_0, p, gap energy, norm gap energy"
  mapM_ putStrLn $ map csvify6 $ zip6 a b c p simpEnergy  normSimpEnergy
    where
      (a,b,c) = unzip3 ins

main = displayData inputs ps simpFirstEnergy normSimpEnergy
  where
    normSimpEnergy = map (uncurry normalize) $ zip a simpFirstEnergy
    (a,_,_) = unzip3 inputs
-- Plot energy band vs band number
--main = mapM_ putStrLn $ [show a ++ ", " ++ show b | (a,b) <- zip [1,2 ..] allowedWidths]

-- Plot band gaps vs band number
--main = mapM_ putStrLn $ [show a ++ ", " ++ show b | (a,b) <- zip [1,2 ..] forbiddenWidths]

-- Plot the characteristic equation
--e = [0.0, 0.01 ..]
--main = mapM_ putStrLn $ [show a ++ ", " ++ show b | (a,b) <- zip e (map sampEqu e)]
