{-# LANGUAGE ScopedTypeVariables #-}

-- | chapter 2: bird & wadler, introduction to functional programming.
-- square root implementation, using a combination of simple functions.
-- Newton's method used to find roots.
-- usage: load this file in GHCi and invoke the `sqrt`, `cubrt` functions.
-- Prem Muthedath, 1 SEP 2021.

--------------------------------------------------------------------------------
module C2Num where

-- | first attempt.
improve' :: Fractional a => a -> a -> a
improve' x y = (y + x/y)/2.0

satis' :: forall a. (Fractional a, Ord a) => a -> a -> Bool
satis' x y = abs (y ^ (2 :: Int)  - x) < eps
  where eps :: a
        eps = 0.0001

until' :: forall a. (a -> Bool) -> (a -> a) -> a -> a
until' p f x | p x = x
             | otherwise = until' p f (f x)

sqrt1 :: (Fractional a, Ord a) => a -> a
sqrt1 x | x < 0 = error "square root computed only for positive numbers."
        | otherwise = until' (satis' x) (improve' x) x

--------------------------------------------------------------------------------
-- | second attempt -- using local functions.
sqrt2 :: forall a. (Fractional a, Ord a) => a -> a
sqrt2 x | x < 0 = error "square root computed only for positive numbers."
        | otherwise = until' satis improve x
  where satis :: a -> Bool
        satis y = abs (y ^ (2 :: Int) - x) < eps
        improve :: a -> a
        improve y = (y + x/y)/2.0
        eps :: a
        eps = 0.0001

--------------------------------------------------------------------------------
-- | third attempt -- more general, uses general definition of Newton's method.
-- `newton` approximates the root of funtion `f` supplied to it.
newton :: forall a. (Fractional a, Ord a) => (a -> a) -> a -> a
newton f = until' satis improve
  where satis :: a -> Bool
        satis y = abs (f y) < eps
        improve :: a -> a
        improve y = (y - f y/deriv y)
        deriv :: a -> a         -- derivative of `f` @ `y`.
        deriv y = (f (y + dy) - f y) / dy
        dy :: a
        dy = 0.0001
        eps :: a
        eps = 0.0001

-- computes square root.
sqrt3 :: forall a. (Fractional a, Ord a) => a -> a
sqrt3 x | x < 0 = error "square root computed only for positive numbers."
        | otherwise = newton f x
  where f :: a -> a
        f y = y ^ (2 :: Int) - x

-- computes cube root.
cubrt :: forall a. (Fractional a, Ord a) => a -> a
cubrt x | x < 0 = error "cube root computed only for positive numbers."
        | otherwise = newton f x
  where f :: a -> a
        f y = y ^ (3 :: Int) - x

--------------------------------------------------------------------------------
