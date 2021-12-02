-- | 8 queens problem.
-- chapter 6, Bird & Wadler: Introduction to Functional Programming (1988).
-- Gill, Launchbury, Simon Peyton Jones: A Shortcut to Deforestation.
-- 1st version: 9 JUL 2021.  Prem Muthedath.

--------------------------------------------------------------------------------
module C6Queens where

-- | returns all possible placements of 8 queens.
-- a placement is a list of length `m`, giving for each column the row in which 
-- queen appears. for example: [4, 6, 1, 5, 2, 8, 3, 7] represents a placement 
-- of 8 queens with queen positions at column 1, row 4; column 2, row 6; column 
-- 3, row 1; ... ; column 8, row 7.
queens :: Int -> [[Int]]
queens 0 = [[]]
queens m = [p ++ [n] | p <- queens (m-1), n <- [1 .. 8], safe p n]

-- | same as `queens` but with a different generational order.
queens' :: Int -> [[Int]]
queens' 0 = [[]]
queens' m = [ p ++ [n] | n <- [1 .. 8], p <- ps, safe p n]
  where ps = queens' (m-1)

-- | given a placement, returns true if the next queen position is safe.
safe :: [Int] -> Int -> Bool
safe p n = and [ not (check (i, j) (m, n)) | (i, j) <- zip [1 .. ] p ]
  where m = length p + 1

-- | returns true if two queen positions are in check.
-- two queen positions will hold each other in check if they are in the same row 
-- or in either of two diagonals.
check :: (Int, Int) -> (Int, Int) -> Bool
check (i, j) (m, n) = (j == n) || (i + j == m + n) || (i - j == m - n)

-- | main
main :: IO ()
main = mapM_ (\x -> print x) $ queens 8

--------------------------------------------------------------------------------
