import Solve
import KP

primaryIntervals func = 
  [search func start 1e-2  | (_, start) <- (0,0):primaryIntervals func]

primaryRoots func =  map (ridder func) (primaryIntervals func)

secondaryIntervals func =
  concat [[(x1, ext x1 x2), (ext x1 x2, x2)] | (x1, x2) <- intervals]
    where
      intervals = zip (primaryRoots func) $ tail $ primaryRoots func
      ext x1 x2 = findExtreme func (x1, x2)

secondaryRoots func =
  map (solve func) $ secondaryIntervals func
  where
    solve func int@(x1, x2) = ridder modFunc int
      where
        modFunc x = func x - offset
        offset = signum $ func $ (x2 + x1) / 2

findExtreme func (start, stop) =
  bisection (numDeriv func) (start, stop)

-- Plot the characteristic equation
--e = [0.0, 0.0001 ..]
--main = mapM_ putStrLn $ [show a ++ ", " ++ show b | (a,b) <- zip e (map equ e)]
--  where
--    equ = charEqu 1e-10 5e-10 3
