module HECC where

data HECC5557 = Point Int Int
            | Infinity
            deriving (Show, Eq, Ord)

instance Num HECC5557 where
  (+) (Point x1 y1) (Point x2 y2)
      | eqModP (y1 + y2 + a1 * x2 + a3) 0 && eqModP x1 x2 = Infinity
      | otherwise                               = Point x3 y3
       where
        x3 = (lambda^ (2::Int)  + a1 * lambda - a2 - x1 - x2) `mod` p
        y3 = (-(lambda + a1) * x3 - nu - a3) `mod` p
        lambda | eqModP x1 x2 = (3 * x1^ (2::Int) + 2 * a2 * x1 + a4 -a1 * y1) `modDiv` (2 * y1 + a1 * x1 + a3)
          | otherwise = (y2 - y1) `modDiv` (x2 - x1)
        nu | eqModP x1 x2  = ((-x1)^ (3::Int) + a4*x1 + 2*a6 - a3*y1) `modDiv` (2*y1 + a1*x1 + a3)
          | otherwise = (y1 * x2 - y2 * x1) `modDiv` (x2 - x1)
        (a1,a2,a3,a4,a6) = (0,0,0,17,173)
        p = 5557
        modDiv alpha beta = (alpha * (beta ^ (p-2 :: Int))) `mod` p
        eqModP alpha beta = alpha `mod` p == beta `mod` p
  (+) p Infinity = p
  (+) a b = (+) b a
  (-) a (Point x2 y2) = (+) a (Point x2 (-1*y2))
  (-) a Infinity = a
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

(×) :: Int -> HECC5557 -> HECC5557
(×) a rho = foldl (+) rho $ replicate (a-1) rho
