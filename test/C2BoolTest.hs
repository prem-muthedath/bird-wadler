{-# LANGUAGE ScopedTypeVariables #-}

-- | quickcheck tests for ../src/C2Bool.hs
-- author: Prem Muthedath, DEC 2021.
-- usage:
--  1. `cd` to `bird-wadler` directory, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler` to start GHCi.
--  3. at GHCi prompt, enter `import C2BoolTest`.
--  4. you can then invoke `runAllQC` to run all quickcheck tests.

--------------------------------------------------------------------------------
module C2BoolTest where

--------------------------------------------------------------------------------
import Test.QuickCheck
import Data.List (sort)

import C2Bool
import QCTest
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | quickcheck testing -- leap year stuff.
--------------------------------------------------------------------------------
-- | property to test equivalence of of `leap` & `leap'` functions.
prop_leap_equiv :: Property
prop_leap_equiv = forAll (chooseInt (1, 3000)) $
  \x -> lClassifys x $
        leap x === leap' x

-- | property to test a leap year.
prop_lyear :: Property
prop_lyear = forAll genLeap $
                \x -> lClassifys x $
                      leap' x === True

-- | property to test a non-leap year.
prop_year :: Property
prop_year = forAll (genYear notLeap) $
                \x -> lClassifys x $
                      leap' x === False

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
-- | quickcheck testing -- triangle stuff
--------------------------------------------------------------------------------
-- | for random valid inputs, check if outputs are valid.
prop_trian_valid :: Property
prop_trian_valid = forAll assorted $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 (analyze a b c) `elem` [0 .. 3]
--------------------------------------------------------------------------------
-- | for random valid inputs, outputs remain same even when inputs are doubled.
prop_trian_double_equiv :: Property
prop_trian_double_equiv = forAll assorted $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c == analyze (2*a) (2*b) (2*c)
--------------------------------------------------------------------------------
-- | property to test outputs for equilateral triangle.
prop_trian_equi :: Property
prop_trian_equi = forAll same $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c === 1
--------------------------------------------------------------------------------
-- | property to test outputs for non-existent triangle.
prop_trian_bad :: Property
prop_trian_bad = forAll bad $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c === 0
  where -- | generate random list of 3-elems that do not form sides of a triangle.
        bad :: Gen [Int]
        bad = genList
                (\(x:y:z:[]) -> (getPositive x + getPositive y <= getPositive z))
                False
--------------------------------------------------------------------------------
-- | property to test outputs for isoceles triangle.
prop_trian_iso :: Property
prop_trian_iso = forAll iso $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c === 2
  where -- | generate random list of 3-elems that form an isoceles triangle.
        iso :: Gen [Int]
        iso = genList
                ((\(x:y:z:[]) -> (x /= z) && ((x == y) || (y == z))))
                True
--------------------------------------------------------------------------------
-- | property to test outputs for scalene triangle.
prop_trian_scal :: Property
prop_trian_scal = forAll scal $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c === 3
  where -- | generate random list of 3-elems that form a scalene triangle.
        scal :: Gen [Int]
        scal = genList (\(x:y:z:[]) -> (x < y) && (y < z)) True
--------------------------------------------------------------------------------
-- | helper functions.

-- | generate random 3-elem, ordered list; some generated lists may have all 
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
runAllQC = qc tests
  where tests :: [(String, Property)]
        tests = [("leap_equivalence", prop_leap_equiv),
                 ("leap year", prop_lyear),
                 ("proper year", prop_year),
                 ("triangle: valid output", prop_trian_valid),
                 ("triangle: double equivalence", prop_trian_double_equiv),
                 ("no triangle", prop_trian_bad),
                 ("equilateral", prop_trian_equi),
                 ("isoceles", prop_trian_iso),
                 ("scalene", prop_trian_scal)
                ]
--------------------------------------------------------------------------------
