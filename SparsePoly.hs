{-# LANGUAGE InstanceSigs #-}
module SparsePoly(fromDP, toDP, qrP) where

import PolyClass
import Representation

import Data.List(sortBy, sortOn)
import Data.Function(on)
import qualified Data.Ord

-- | fromDP example
-- >>> fromDP sampleDP
-- S {unS = [(3,1),(0,-1)]}
fromDP :: (Eq a, Num a) => DensePoly a -> SparsePoly a
fromDP (P []) = S []
fromDP (P coeffs) = S $ normalize $ filter ((/= 0) . snd) $ zip [0..] coeffs

toDP :: (Eq a, Num a) => SparsePoly a -> DensePoly a
toDP (S []) = P []
toDP (S xs) = P $ map snd xs ++ replicate (fst (last xs) - length xs + 1) 0

first :: (a -> a') -> (a, b) -> (a', b)
first f (a, b) = (f a, b)

second :: (b -> b') -> (a, b) -> (a, b')
second f (a, b) = (a, f b)

instance Functor SparsePoly where
  fmap :: (a -> b) -> SparsePoly a -> SparsePoly b
  fmap f (S xs) = S $ map (second f) xs

instance Polynomial SparsePoly where
  zeroP :: SparsePoly a
  zeroP = S []

  constP :: (Eq a, Num a) => a -> SparsePoly a
  constP x = S $ normalize [(0, x)]

  varP :: Num a => SparsePoly a
  varP = S [(1, 1)]

  evalP :: Num a => SparsePoly a -> a -> a
  evalP (S []) _ = 0
  evalP (S xs) y = sum [c * y ^ i | (i, c) <- xs]

  shiftP :: (Eq a, Num a) => Int -> SparsePoly a -> SparsePoly a
  shiftP n (S xs) = S [(i + n, c) | (i, c) <- xs]

  degree :: (Eq a, Num a) => SparsePoly a -> Int
  degree (S []) = -1
  degree (S xs) = fst $ last xs

instance (Eq a, Num a) => Num (SparsePoly a) where
  (+), (*) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> SparsePoly a
  (+) (S xs) (S ys) = S $ sortPairsByFirstDesc $ cleanUp $ combine (+) xs ys
  (*) (S xs) (S ys) = S $ normalize [(i+j, c*d) | (i, c) <- xs, (j, d) <- ys]

  negate :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
  negate (S xs) = S $ map (second negate) xs

  fromInteger :: (Eq a, Num a) => Integer -> SparsePoly a
  fromInteger x = constP $ fromInteger x

  abs :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
  abs (S coeffs) = S $ map (first abs) coeffs

  signum :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
  signum = undefined

sortPairsByFirstDesc :: Ord a => [(a, b)] -> [(a, b)]
sortPairsByFirstDesc = sortBy (\(a, _) (b, _) -> compare b a)

cleanUp :: (Eq b, Num b) => [(a, b)] -> [(a, b)]
cleanUp = filter (\(_, y) -> y /= 0)

combine :: Eq a => (b -> b -> b) -> [(a, b)] -> [(a, b)] -> [(a, b)]
combine f xs ys =
    let common = [(x, f a b) | (x, a) <- xs, (y, b) <- ys, x == y]
        unique = filter (\(x, _) -> x `notElem` map fst ys) xs ++ filter (\(x, _) -> notElem x (map fst xs)) ys
        in  unique ++ common

normalize :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)]
normalize xs = filter (\(_, c) -> c /= 0) $ simplify $ sortOn (Data.Ord.Down . fst) xs

simplify :: (Eq a, Num b) => [(a, b)] -> [(a, b)]
simplify [] = []
simplify [(a,b)] = [(a,b)]
simplify ((a,b):(c,d):xs) 
  | a == c = simplify $ (a, b + d):xs
  | otherwise = (a,b): simplify ((c,d):xs)


instance (Eq a, Num a) => Eq (SparsePoly a) where
    (==) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> Bool
    p == q = nullP(p-q)

-- qrP s t | not(nullP t) = (q, r) iff s == q*t + r && degree r < degree t
qrP :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrP = undefined

-- | Division example
-- >>> qrP (x^2 - 1) (x -1) == ((x + 1), 0)
-- True
