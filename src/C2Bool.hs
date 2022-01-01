{-# LANGUAGE ScopedTypeVariables #-}

-- | chapter 2: bird & wadler, introduction to functional programming.
-- boolean data type: examples.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler` to start GHCi.
--  3. next, at GHCi prompt, enter `import C2Bool`.
--  4. you can then invoke `leap` & `analyze` functions.
--  5. OR instead of step 2,
--      a) run `cabal v2-repl :bird-wadler-test` to start GHCi;
--      b) at GHCi prompt, enter `import C2BoolTest`;
--      c) invoke `C2BoolTest.ghciQC` to run all quickcheck tests.
-- author: Prem Muthedath, 19 OCT 2021.

--------------------------------------------------------------------------------
module C2Bool where
--------------------------------------------------------------------------------
-- | NOTES (from bord & wadler)
-- we need to compare things: e.g., compare numbers. for this, we need 
-- "truth-values." there are 2 canonical expressions for truth-values; `True` 
-- and `False`. these 2 expressions constitute the data type `Bool`.

-- a function that returns boolean values is called a predicate.

-- booleans are important because they are results returned by comparison 
-- operators.
--    ==    equals
--    /=    not equals
--    <     less than
--    >     greater than
--    <=    less than or equals
--    >=    greater than or equals

-- all 6 comparison operators have the same level of precedence, which is lower 
-- than that of arithmetic operators.
--    2 == 3      => False
--    2 < 1 + 3   => True
--
-- comparison operators can take expressions of arbitrary types as arguments.  
-- the only restriction is that the two arguments MUST have the same type. each 
-- comparison operator is therefore a polymorphic function with type:
--
--    a -> a -> Bool
--
-- comparison operators on booleans are defined so that `False` is < `True`.

-- equality tests on numbers may not return the correct result unless the 
-- numbers are integers within the permitted range. fractional numbers should 
-- only be compared up to a specified tolerance. better to define:
--
--    within eps x y = abs (x - y) < eps
--
-- and use `within eps` instead of (=) as a more realistic equality test on 
-- fractional numbers.
--
-- in essence, the evaluator compares the result of an equality test of the form 
-- `e1 = e2` by reducing the expressions `e1` and `e2` to their canonical form 
-- and testing whether the two results are identical.  if the expressions do not 
-- have a cononical representation, the the result of the test is the undefined 
-- value _|_. in particular, function values have no canonical representation, 
-- so testing functions for equality always results in _|_. for example,
--
--    double x = x + x
--    square x = x * x
--
--    (double == square) = _|_
--
-- in math, `double = square` is a false statement, where as here it is _|_; 
-- also, in math, `_|_ = _|_` is a true statement, where as here it is _|_. this 
-- does does not mean our evaluator is a non-mathematical machine, just that its 
-- behavior is defined by a different set of mathematical rules, rules designed 
-- to be executed mechanically.
--
-- boolean values may be combined using the following logical operators:
--    &&    => conjunction or logical `and`
--    ||    => dijunction or logical 'or'
--    not   => negation of logical `not` -- not an operator but a function
--
-- rules of precedence: `&&` binds more tightly than `||`, and of course, since 
-- `not` is a function, it binds tightest. however, it is best to use brackets 
-- whenever you use logical connectives.

--------------------------------------------------------------------------------
-- | Haskell Report 2010, Chapter 6.
-- https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1160006

-- class  Eq a  where
--     (==), (/=)  ::  a -> a -> Bool
--
--     x /= y  = not (x == y)
--     x == y  = not (x /= y)

-- data  Ordering  =  LT | EQ | GT
--          deriving (Eq, Ord, Enum, Read, Show, Bounded)

-- class  (Eq a) => Ord a  where
--    compare              :: a -> a -> Ordering
--    (<), (<=), (>=), (>) :: a -> a -> Bool
--    max, min             :: a -> a -> a
--
--    compare x y | x == y    = EQ
--                | x <= y    = LT
--                | otherwise = GT
--
--    x <= y  = compare x y /= GT
--    x <  y  = compare x y == LT
--    x >= y  = compare x y /= LT
--    x >  y  = compare x y == GT
--
--    -- Note that (min x y, max x y) = (x,y) or (y,x)
--    max x y | x <= y    =  y
--            | otherwise =  x
--    min x y | x <= y    =  x
--            | otherwise =  y

--------------------------------------------------------------------------------
-- | Haskell Report 2010, Chapter 9.
-- https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1710009

-- | boolean operators.
-- infix  4  ==, /=, <, <=, >=, >
-- infixr 3  &&
-- infixr 2  ||

-- | defined in `GHC.Types`.
-- data  Bool  =  False | True deriving (Eq, Ord, Enum, Read, Show, Bounded)

-- (&&), (||)   :: Bool -> Bool -> Bool
-- True  && x   =  x
-- False && _   =  False
-- True  || _   =  True
-- False || x   =  x

-- not        :: Bool -> Bool
-- not True   =  False
-- not False  =  True

-- otherwise :: Bool
-- otherwise =  True

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | leap year calculation.
-- leap year: divisible by 4, & if divisible by 100, then divisible by 400.

-- | years for testing leap year calculation.
-- the years 1600, 2000 are leap years.
-- the years 1700, 1800, 1900 all divisible by 4 but not leap years.
-- for example, test in GHCi by running this code: `map leap years`.
years :: [Int]
years = [1600, 2000, 1700, 1800, 1900]

-- | first attempt, using boolean operators.
leap :: Int -> Bool
leap y | y <= 0 = error "year has to be >= 1"
leap y = (y `mod` 4 == 0) && ((y `mod` 100 /= 0) || (y `mod` 400 == 0))

-- | second attempt, using guards & local defn.
leap' :: Int -> Bool
leap' y | y <= 0    = error "year has to be >= 1"
        | byHundred = y `mod` 400 == 0
        | otherwise = y `mod` 4 == 0
        where byHundred :: Bool
              byHundred = y `mod` 100 == 0

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | triangle construction.
-- given three positive numbers `a`, `b`, `c` in non-decreasing order, 
-- representing the lengths of a possible triangle, the function `analyze` 
-- returns one of the numbers 0, 1, 2, or 3 depending on whether the sides:
--    (i)   0 -- do not form a proper triangle; i.e., `c` >= `a` + `b`;
--    (ii)  1 -- form an equilateral triangle;
--    (iii) 2 -- form an isoceles triangle (2 lengths are equal);
--    (iv)  3 -- form a scalene triangle (all lengths are different).
analyze :: Int -> Int -> Int -> Int
analyze a b c | a <= 0 || a > b || b > c
                = error "input must be 3 positive numbers in non-decreasing order."
              | (a + b) <= c = 0                                    -- improper
              | (a + b > c) && (a == c) = 1                         -- equilateral
              | (a + b > c) && (a /= c && (a == b || b == c)) = 2   -- isoceles
              | (a + b > c) && (a < b && b < c) = 3                 -- scalene
              | otherwise = error "unable to analyze input."

--------------------------------------------------------------------------------
