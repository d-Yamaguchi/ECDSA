module HECC where

import Data.Maybe

data HECC5557 = Point Integer Integer
            | Infinity
            deriving (Show, Eq, Ord)

instance Num HECC5557 where
  (+) (Point x1 y1) (Point x2 y2)
      | eqModP (y1 + y2 + a1 * x2 + a3) 0 && eqModP x1 x2 = Infinity
      | otherwise                               = Point x3 y3
       where
        x3 = (lambda^ (2::Integer)  + a1 * lambda - a2 - x1 - x2) `mod` p
        y3 = (-(lambda + a1) * x3 - nu - a3) `mod` p
        lambda | eqModP x1 x2 = (3 * x1^ (2::Integer) + 2 * a2 * x1 + a4 -a1 * y1) `modDiv` (2 * y1 + a1 * x1 + a3)
               | otherwise = (y2 - y1) `modDiv` (x2 - x1)
        nu | eqModP x1 x2  = (- x1^ (3::Integer) + a4*x1 + 2*a6 - a3*y1) `modDiv` (2*y1 + a1*x1 + a3)
           | otherwise = (y1 * x2 - y2 * x1) `modDiv` (x2 - x1)
        (a1,a2,a3,a4,a6) = (0,0,0,17,173)
        p = 5557
        modDiv alpha beta = (alpha * fromJust (modInv (beta `mod` p) p)) `mod` p
        eqModP alpha beta = alpha `mod` p == beta `mod` p
  (+) p Infinity = p
  (+) a b = (+) b a
  (-) a (Point x2 y2) = (+) a (Point x2 (-1*y2))
  (-) a Infinity = a
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

(×) :: Integer -> HECC5557 -> HECC5557
(×) a rho = foldl (+) rho $ replicate (fromIntegral a-1) rho

gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)

modInv a m = let (i, _, g) = gcdExt a m
             in if g == 1 then Just (mkPos i) else Nothing
  where mkPos x = if x < 0 then x + m else x

basePoint :: (Integer,Integer)
basePoint = head $ filter infCondition [(x,y)|x<-[1..p],y<-[1..p]]
  where
    eqMod5557 alpha beta = alpha `mod` p == beta `mod` p
    infCondition l = (snd l^2) `eqMod5557` (fst l^3 + 17*fst l + 173)
    p = 5557

orderN :: Integer -> HECC5557 -> Integer
orderN n Infinity = n
orderN n p
     | n == 5000 = -1
     | otherwise = orderN (n + 1) (p + g)

g = let gSet = uncurry Point in gSet basePoint
