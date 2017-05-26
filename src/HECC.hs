module HECC where

data HECC5557 = Point Int Int
            | Infinity
            deriving (Show, Eq, Ord)

instance Num HECC5557 where
  (+) (Point x1 y1) (Point x2 y2)
      | eqModP (y1 + y2 + a1 * x2 + a3) 0 && eqModP x1 x2 = Infinity
      | otherwise                               = Point x3 y3
       where
        x3 = (lambda^ (2::Int)  + a1 * lambda - a2 - x1 - x2) `mod` 17
        y3 = (-(lambda + a1) * x3 - nu - a3) `mod` 17
        lambda | eqModP x1 x2 = (3 * x1^ (2::Int) + 2 * a2 * x1 + a4 -a1 * y1) `modDiv` (2 * y1 + a1 * x1 + a3)
          | otherwise = (y2 - y1) `modDiv` (x2 - x1)
        nu | eqModP x1 x2  = ((-x1)^ (3::Int) + a4*x1 + 2*a6 - a3*y1) `modDiv` (2*y1 + a1*x1 + a3)
          | otherwise = (y1 * x2 - y2 * x1) `modDiv` (x2 - x1)
        (a1,a2,a3,a4,a6) = (0,0,0,17,173)
        modDiv alpha beta = head $ filter (\gamma -> alpha `eqModP` (gamma * beta)) [1..5557]
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

eqModP alpha beta = alpha `mod` 5557 == beta `mod` 5557
