module DensePoly() where
import PolyClass
import Representation

instance Functor DensePoly where
  fmap f (P coeffs) = P $ map f coeffs

instance Polynomial DensePoly where
  zeroP = P []
  constP x = P [x]
  varP = P [0, 1]
  evalP (P coeffs) x = sum $ zipWith (\c i -> c * x^i) coeffs [0..]
  shiftP n (P coeffs) = P $ replicate n 0 ++ coeffs
  degree (P []) = -1
  degree (P coeffs) = length coeffs - 1


instance (Eq a, Num a) => Num (DensePoly a) where
  (P coeffs1) + (P coeffs2) = normalize $ P $ addCoeffs coeffs1 coeffs2
    where
      addCoeffs [] ys = ys
      addCoeffs xs [] = xs
      addCoeffs (x:xs) (y:ys) = (x+y) : addCoeffs xs ys

  negate (P coeffs) = P $ map negate coeffs

  (P coeffs1) * (P coeffs2) = normalize $ P $ multCoeffs coeffs1 coeffs2
    where
      multCoeffs [] _ = []
      multCoeffs _ [] = []
      multCoeffs (x:xs) ys = addCoeffs (map (x*) ys) (0 : multCoeffs xs ys)

      addCoeffs [] ys = ys
      addCoeffs xs [] = xs
      addCoeffs (x:xs) (y:ys) = (x+y) : addCoeffs xs ys

  fromInteger 0 = P []
  fromInteger n = P [fromInteger n]

  abs (P coeffs) = P $ map abs coeffs


normalize (P []) = P []
normalize (P coeffs) = P $ reverse $ dropWhile (== 0) $ reverse coeffs

-- |
-- >>> x^3 - 1 :: DensePoly Integer 
-- P {unP = [-1,0,0,1]}

-- | Num operations give canonical results:
-- >>> isCanonicalDP (sampleDP - sampleDP)
-- True
    
instance (Eq a, Num a) => Eq (DensePoly a) where
  (P xs) == (P ys) = xs == ys

-- |
-- >>>  P [1,2] == P [1,2]
-- True

-- |
-- >>> fromInteger 0 == (zeroP :: DensePoly Int)
-- True

-- |
-- >>>  P [0,1] == P [1,0]
-- False

-- | Degree examples
-- >>> degree (zeroP :: DensePoly Int)
-- -1
-- >>> degree (constP 1 :: DensePoly Int)
-- 0
