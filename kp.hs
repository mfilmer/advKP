module KP where

import Newton

-- constants
m = 9.109e-31       -- [kg]
q = 1.602e-19       -- [C]
hbar = 1.0546e-34   -- [J s]

-- KP model characteristic equation
-- a   -> well width      [m]
-- b   -> barrier width   [m]
-- v_0 -> barrier height  [eV]
-- e   -> energy          [eV]
charEqu a b v_0 0 = charEqu a b v_0 1e-16
charEqu a b v_0eV eeV
  | xi < 1 = (1-2*xi)/(2*sqrt(xi*(1-xi)))*sin(a_0*a*sqrt xi)*sinh(a_0*b*sqrt(1-xi)) + cos(a_0*a*sqrt(xi))*cosh(a_0*b*sqrt(1-xi))
  |otherwise = (1-2*xi)/(2*sqrt(xi*(xi-1)))*sin(a_0*a*sqrt xi)*sin(a_0*b*sqrt(xi-1)) + cos(a_0*a*sqrt(xi))*cos(a_0*b*sqrt(xi-1))
    where
      a_0 = sqrt (2*m*v_0/hbar**2)
      xi = e / v_0
      v_0 = evToJ v_0eV
      e = evToJ eeV

-- Convert between eV and J
evToJ eV = eV * q
jToEv j = j / q

-- Other functions
log10 = logBase 10
linspace start stop count = [start, start + step .. stop]
  where
    step = (stop - start) / (count - 1)
logspace start stop count = [10**x | x <- linspace lstart lstop count]
  where
    lstart = log10 start
    lstop = log10 stop