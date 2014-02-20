import Solve
import KP

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

diffs :: Num a => [a] -> [a]
diffs [] = []
diffs (_:[]) = []
diffs (a:b:[]) = [(b-a)]
diffs (a:b:xs) = (b-a):diffs xs

group :: [a] -> [(a, a)]
group [] = []
group (_:[]) = []
group (a:b:[]) = [(a,b)]
group (a:b:xs) = (a,b):group xs

-- Do actual data generation
sampEqu = charEqu 1e-10 5e-10 3

allowedBands = group $ secondaryRoots sampEqu
forbiddenRegions = group $ tail $ secondaryRoots sampEqu

allowedWidths = [b - a | (a,b) <- allowedBands]
forbiddenWidths = [b - a | (a,b) <- forbiddenRegions]


-- Plot energy band vs band number
--main = mapM_ putStrLn $ [show a ++ ", " ++ show b | (a,b) <- zip [1,2 ..] allowedWidths]

-- Plot band gaps vs band number
--main = mapM_ putStrLn $ [show a ++ ", " ++ show b | (a,b) <- zip [1,2 ..] forbiddenWidths]

-- Plot the characteristic equation
e = [0.0, 0.0001 ..]
main = mapM_ putStrLn $ [show a ++ ", " ++ show b | (a,b) <- zip e (map sampEqu e)]
