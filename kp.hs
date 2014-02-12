import newton

-- constants
m = 9.109e-31       -- [kg]
q = 1.602e-19       -- [C]
hbar = 1.0546e-34   -- [J s]

-- KP model characteristic equation
-- a   -> well width
-- b   -> barrier width
-- v_0 -> barrier height
-- e   -> energy
charEqu a b v_0 e = 
  (1-2*eps)/(2*sqrt(eps*(eps-1)))*sin(a_0*a*sqrt eps)*sinh(a_0*b*sqrt(abs(eps-1))) +
    cos(a_0*a*sqrt(eps))*cosh(a_0*b*sqrt(abs(eps-1)))
    where
      eps = e/v_0
      a_0 = sqrt (2*m*v_0/hbar**2)

-- Convert between eV and J
evToJ eV = eV * q
jToEv j = j / q