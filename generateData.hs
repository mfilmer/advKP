import Solve
import KP

primaryIntervals a b v_0 =
  [search (charEqu a b v_0) start 1e-2  | (_, start) <- (0,0):primaryIntervals a b v_0]
primaryRoots a b v_0 =  map (ridder (charEqu a b v_0)) (primaryIntervals a b v_0)

-- Function to calculate two secondary roots given bounds
-- func -> function of one variable where func(start) == func(stop) == 0
-- start, stop -> beginning and ending respectively of the region to solve
calcSecondaryRoots func (start, stop) =
  undefined

-- Find the location of the extreme value (max or min) 
-- of the function on the interval (start, stop)
findExtreme func (start, stop) =
  ridder (numDeriv func) (start, stop)

-- Plot the characteristic equation
--e = [0.0, 0.0001 ..]
--main = mapM_ putStrLn $ [show a ++ ", " ++ show b | (a,b) <- zip e (map equ e)]
--  where
--    equ = charEqu 1e-10 5e-10 3
