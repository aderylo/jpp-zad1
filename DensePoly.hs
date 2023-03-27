{-# LANGUAGE InstanceSigs #-}

module DensePoly () where

import PolyClass
import Representation

instance Functor DensePoly where
  fmap f (P coeffs) = P $ map f coeffs

instance Polynomial DensePoly where
  zeroP :: DensePoly a
  zeroP = P []

  constP :: (Eq a, Num a) => a -> DensePoly a
  constP 0 = P []
  constP x = normalize $ P [x]

  varP :: Num a => DensePoly a
  varP = P [0, 1]

  evalP :: Num a => DensePoly a -> a -> a
  evalP (P coeffs) x = sum $ zipWith (\c i -> c * x ^ i) coeffs [0 ..]

  shiftP :: (Eq a, Num a) => Int -> DensePoly a -> DensePoly a
  shiftP n (P coeffs) = normalize $ P $ replicate n 0 ++ coeffs

  degree :: (Eq a, Num a) => DensePoly a -> Int
  degree (P []) = -1
  degree (P coeffs) = length coeffs - 1

instance (Eq a, Num a) => Num (DensePoly a) where
  (+), (*) :: (Eq a, Num a) => DensePoly a -> DensePoly a -> DensePoly a
  (P coeffs1) + (P coeffs2) = normalize $ P $ addCoeffs coeffs1 coeffs2
    where
      addCoeffs [] ys = ys
      addCoeffs xs [] = xs
      addCoeffs (x : xs) (y : ys) = (x + y) : addCoeffs xs ys

  (P coeffs1) * (P coeffs2) = normalize $ P $ multCoeffs coeffs1 coeffs2
    where
      multCoeffs [] _ = []
      multCoeffs _ [] = []
      multCoeffs (x : xs) ys = addCoeffs (map (x *) ys) (0 : multCoeffs xs ys)

      addCoeffs [] ys = ys
      addCoeffs xs [] = xs
      addCoeffs (x : xs) (y : ys) = (x + y) : addCoeffs xs ys

  negate :: (Eq a, Num a) => DensePoly a -> DensePoly a
  negate (P coeffs) = normalize $ P $ map negate coeffs

  fromInteger :: (Eq a, Num a) => Integer -> DensePoly a
  fromInteger 0 = P []
  fromInteger n = P [fromInteger n]

  abs :: (Eq a, Num a) => DensePoly a -> DensePoly a
  abs (P coeffs) = P $ map abs coeffs

  signum :: (Eq a, Num a) => DensePoly a -> DensePoly a
  signum = undefined

normalize :: (Eq a, Num a) => DensePoly a -> DensePoly a
normalize (P []) = P []
normalize (P coeffs) = P $ reverse $ dropWhile (== 0) $ reverse coeffs

-- |
-- >>> x^3 - 1 :: DensePoly Integer
-- P {unP = [-1,0,0,1]}

-- | Num operations give canonical results:
-- >>> isCanonicalDP (sampleDP - sampleDP)
-- True
instance (Eq a, Num a) => Eq (DensePoly a) where
  (==) :: (Eq a, Num a) => DensePoly a -> DensePoly a -> Bool
  (==) (P xd) (P yd) = 
    let P xs = normalize (P xd) 
        P ys = normalize (P yd) 
    in xs == ys

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
