{-# LANGUAGE ScopedTypeVariables #-}

-- | chapter 2: bird & wadler, introduction to functional programming.
-- boolean data type: examples.
-- usage: load this file in GHCi and invoke `leap` & `analyze` functions.
-- Prem Muthedath, 19 OCT 2021.

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
module C2Bool where

import Test.QuickCheck
import Data.List (sort)
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
leap y = (y `mod` 4 == 0) && ((y `mod` 100 /= 0) || (y `mod` 400 == 0))

-- | second attempt, using guards & local defn.
leap' :: Int -> Bool
leap' y | y <= 0    = error "year has to be >= 1"
        | byHundred = y `mod` 400 == 0
        | otherwise = y `mod` 4 == 0
        where byHundred :: Bool
              byHundred = y `mod` 100 == 0

--------------------------------------------------------------------------------
-- | quickcheck testing -- leap year stuff.
--------------------------------------------------------------------------------
-- | property to test a leap year.
prop_lyear :: Property
prop_lyear = forAll genLeap $
                \x -> lClassifys x $
                      (leap' x == True) && (leap x == True)

-- | property to test a non-leap year.
prop_year :: Property
prop_year = forAll (genYear notLeap) $
                \x -> lClassifys x $
                      (leap' x == False) && (leap x == False)

-- | classification of property used in leap year testing.
lClassifys :: (Testable prop) => Int -> prop -> Property
lClassifys x = classify (x <= 100) "<= 100" .
               classify (x > 100 && x <= 1000) "100 - 1000" .
               classify (x > 1000 && x <= 2000) "1000 - 2000" .
               classify (x > 2000 && x <= 3000) "2000 - 3000" .
               classify (x `mod` 100 == 0) "has 00" .
               classify (x `mod` 100 /= 0) "has no 00"

-- | generate a leap year.
genLeap :: Gen Int
genLeap = frequency [(3, (genYear leap4)), (4, (genYear leap100))]

-- | generate a year that satifies the predicate.
genYear :: (Int -> Bool) -> Gen Int
genYear f = (chooseInt (1, 3000)) `suchThat` f

-- | `True` for a leap year divisible by both 100 & 400.
leap100 :: Int -> Bool
leap100 x = (x `mod` 100 == 0) && (x `mod` 400 == 0)

-- | `True` for a leap year divisible only by 4.
leap4 :: Int -> Bool
leap4 x = (x `mod` 4 == 0) && (x `mod` 100 /= 0)

-- | `True` if not a leap year.
notLeap :: Int -> Bool
notLeap x = (not (leap100 x)) && (not (leap4 x))

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
-- | quickcheck testing -- triangle stuff
--------------------------------------------------------------------------------
-- | for random valid inputs, check if outputs are valid.
prop_valid :: Property
prop_valid = forAll assorted $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 (analyze a b c) `elem` [0 .. 3]
--------------------------------------------------------------------------------
-- | for random valid inputs, outputs remain same even when inputs are doubled.
prop_double_equiv :: Property
prop_double_equiv = forAll assorted $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c == analyze (2*a) (2*b) (2*c)
--------------------------------------------------------------------------------
-- | property to test outputs for equilateral triangle.
prop_equi :: Property
prop_equi = forAll same $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c == 1
--------------------------------------------------------------------------------
-- | property to test outputs for non-existent triangle.
prop_bad :: Property
prop_bad = forAll bad $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c == 0
--------------------------------------------------------------------------------
-- | property to test outputs for isoceles triangle.
prop_iso :: Property
prop_iso = forAll iso $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c == 2
--------------------------------------------------------------------------------
-- | property to test outputs for scalene triangle.
prop_scal :: Property
prop_scal = forAll scal $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c == 3
--------------------------------------------------------------------------------
-- | helper functions.

-- | generate random 3-elem, ordered list; some genrated lists may have all 
-- elements identical, while others not.
assorted :: Gen [Int]
assorted = frequency [(1, same), (4, diff)]

-- | generate random list (3-elem) that have all elements same.
same :: Gen [Int]
same = do
  x :: Int <- chooseInt (1, 1000)
  return $ replicate 3 x

-- | generate random list (3-elem, ordered) that do not have all elems same.
diff :: Gen [Int]
diff = genList (\(x:_:z:[]) -> x /= z) False

-- | generate random list of 3-elems that do not form sides of a triangle.
bad :: Gen [Int]
bad = genList
        (\(x:y:z:[]) -> (getPositive x + getPositive y <= getPositive z))
        False

-- | generate random list of 3-elems that form an isoceles triangle.
iso :: Gen [Int]
iso = genList
        ((\(x:y:z:[]) -> (x /= z) && ((x == y) || (y == z))))
        True

-- | generate random list of 3-elems that form a scalene triangle.
scal :: Gen [Int]
scal = genList (\(x:y:z:[]) -> (x < y) && (y < z)) True

-- | generate a random 3-elem, ordered +ve `Int` list.
-- `f` acts as a filter to determine what alements can be included.
-- `bool = True` ensures `x + y < z` in the generated list `[x, y, z]`.
genList :: ([Positive Int] -> Bool) -> Bool -> Gen [Int]
genList f bool = do
  list :: [Positive Int] <- genList'
  return $ map getPositive list
  where genList' :: Gen [Positive Int]
        genList' = listOf3 `suchThat` h
        h :: [Positive Int] -> Bool
        h xs = case bool of
          True  -> (f xs) && (g xs)
          False -> f xs
        g :: [Positive Int] -> Bool
        g (x:y:z:[]) = (getPositive x + getPositive y > getPositive z)
        g _          = error "not a 3 elem list"
        listOf3 :: Gen [Positive Int]
        listOf3 = sort <$> vectorOf 3 (arbitrary :: Gen (Positive Int))

-- | classifications for a property used in testing triangle construction.
classifys :: (Testable prop) => [Int] -> prop -> Property
classifys xs@(a:b:c:[]) = classify (length xs == 3) "3-elem list input" .
                          classify (a + b <= c) "bad" .
                          classify (a + b > c) "triangle" .
                          classify (a == c) "equilateral"
classifys _             = error "need exactly a 3-element list."

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | run all `QuickCheck` tests.
runAllQC :: IO ()
runAllQC = mapM_ runQC tests
  where -- | run `QuickCheck` test case.
        runQC :: (String, Property) -> IO ()
        runQC (x, y) = do
             putStrLn $ "\n--- " <> x <> " ---"
             quickCheck y
        tests :: [(String, Property)]
        tests = [("leap year", prop_lyear),
                 ("proper year", prop_year),
                 ("valid", prop_valid),
                 ("double equivalence", prop_double_equiv),
                 ("no triangle", prop_bad),
                 ("equilateral", prop_equi),
                 ("isoceles", prop_iso),
                 ("scalene", prop_scal)
                ]
--------------------------------------------------------------------------------
