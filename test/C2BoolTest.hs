{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | quickcheck tests for ../src/C2Bool.hs
-- author: Prem Muthedath, DEC 2021.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler-test` to start GHCi.
--  3. at GHCi prompt, enter `import C2BoolTest`.
--  4. you can then invoke `C2BoolTest.ghciQC` to run all quickcheck tests.

--------------------------------------------------------------------------------
module C2BoolTest where
--------------------------------------------------------------------------------
import Test.QuickCheck
import Data.List (sort)

import C2Bool
import Common (ghciRunner)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | quickcheck testing -- leap year stuff.
--------------------------------------------------------------------------------
-- | property to test equivalence of of `leap` & `leap'` functions.
prop_leap_equiv :: Property
prop_leap_equiv = forAll (chooseInt (1, 3000)) $
  \x -> leap_classifys x $
        leap x === leap' x

-- | check if leap year generator is valid.
prop_validLeap :: Property
prop_validLeap = forAll genLeap $
  \x -> leap_classifys x $
        (x `mod` 4 === 0 .&&. x `mod` 100 =/= 0) .||.
        (x `mod` 100 === 0 .&&. x `mod` 400 === 0)

-- | check if non-leap year generator is valid.
prop_validNonLeap :: Property
prop_validNonLeap = forAll (genYear notLeap) $
  \x -> leap_classifys x $
        (x `mod` 4 =/= 0) .||. (x `mod` 100 =/= 0  .||.  x `mod` 400 =/= 0)

-- | property to test a leap year.
prop_leap :: Property
prop_leap = forAll genLeap $
              \x -> leap_classifys x $
                    leap' x === True

-- | property to test a non-leap year.
prop_non_leap :: Property
prop_non_leap = forAll (genYear notLeap) $
                  \x -> leap_classifys x $
                        leap' x === False

-- | classification of property used in leap year testing.
leap_classifys :: (Testable prop) => Int -> prop -> Property
leap_classifys x = classify (x <= 100) "<= 100" .
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
-- | `Triangle` data type.
data Triangle = Equilateral
                | Isoceles
                | Scalene
                | Good   -- 3 sides form a triangle.
                | Bad    -- 3 sides do NOT form a triangle.
                | Mix    -- may be good, may be bad; but NOT all sides same.
                deriving (Show, Eq, Enum)
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `Triangle`.
instance Arbitrary (Triangle) where
  arbitrary = elements [toEnum 0 :: Triangle ..]
--------------------------------------------------------------------------------
-- | check if `Triangle` generator is valid.
prop_validTriangle :: Property
prop_validTriangle = forAll (arbitrary :: Gen Triangle) $
  \x -> x `elem` [toEnum 0 :: Triangle .. ]
--------------------------------------------------------------------------------
-- | check if lists generated are sorted, have right size, and elems are > 0.
prop_trian_validList :: Property
prop_trian_validList = forAll (assorted :: Gen [Int]) $
                          \xs -> classifys xs $
                                 (sort xs === xs) .&&.
                                 (length xs === 3) .&&.
                                 (all (> 0) xs)
--------------------------------------------------------------------------------
-- | for random valid inputs, check if outputs are valid.
prop_trian_assorted :: Property
prop_trian_assorted = forAll assorted $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 (analyze a b c) `elem` [0 .. 3]
--------------------------------------------------------------------------------
-- | for random valid inputs, outputs remain same even when inputs are doubled.
prop_trian_double :: Property
prop_trian_double = forAll assorted $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c === analyze (2*a) (2*b) (2*c)
--------------------------------------------------------------------------------
-- | property to test outputs for non-existent triangle.
prop_trian_bad :: Property
prop_trian_bad = forAll (genList Bad) $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c === 0
--------------------------------------------------------------------------------
-- | property to test outputs for equilateral triangle.
prop_trian_equi :: Property
prop_trian_equi = forAll (genList Equilateral) $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c === 1
--------------------------------------------------------------------------------
-- | property to test outputs for isoceles triangle.
prop_trian_iso :: Property
prop_trian_iso = forAll (genList Isoceles) $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c === 2
--------------------------------------------------------------------------------
-- | property to test outputs for scalene triangle.
prop_trian_scal :: Property
prop_trian_scal = forAll (genList Scalene) $
  \(a:b:c:[]) -> classifys [a, b, c] $
                 analyze a b c === 3
--------------------------------------------------------------------------------
-- | helper functions.

-- | generate random 3-elem ordered list; all elements are +ve integers.
-- some lists may represent sides of a real triangle, while others not.
assorted :: Gen [Int]
assorted = do
  triangle :: Triangle <- (arbitrary :: Gen Triangle)
  genList triangle

-- | generate 3-elem, sorted, random list whose elements satisfy the property 
-- associated with the supplied `Triangle`. list elements are all +ve integers.
genList :: Triangle -> Gen [Int]
-- NOTE: for `Equilateral`, because getting a list of identical elements through 
-- random sampling is a rare event, it takes a while to generate an 
-- `Equilateral` triangle like the way other triangles are generated. so we use 
-- a different algorithm to generate an `Equilateral` triangle.
genList Equilateral = (replicate 3) <$> chooseInt (1, 1000)
genList triangle    = listOf3 `suchThat` f
  where f :: ([Int] -> Bool)
        f | triangle `elem` [Mix, Bad] = g
          | otherwise                  = \x -> g x && h x
        g :: ([Int] -> Bool)
        g = property' triangle
        h :: ([Int] -> Bool)
        h = property' Good
        -- | sorted random list of 3 elements. all elements are +ve integers.
        listOf3 :: Gen [Int]
        listOf3 = do
            xs :: [Positive Int] <- sort <$> vectorOf 3 (arbitrary :: Gen (Positive Int))
            return $ map getPositive xs

-- return property (a lambda) associated with supplied `Triangle`.
-- NOTE: in case of valid triangles other than `Good`, the returned property 
-- does NOT include the basic check of whether or not something is a triangle.  
-- if you need to generate a valid triangle, use `genList` given above.
-- NOTE: the properties (i.e., lamdas) ASSUME that the `list` is SORTED.
property' :: Triangle -> ([Int] -> Bool)
-- we define somewhat complex & ingenious triangle properties, because we do not 
-- want to replicate the source code here, and in that way duplicate bugs!
property' Equilateral = \xs@(x:_:_:[]) -> sum xs == 3 * x
property' Isoceles    = \xs@(x:y:z:[]) ->
                          (sum xs /= 3 * x) &&
                          ((sum xs == 2 * y + z) ||
                           (sum xs == x + 2 * y))
property' Scalene     = \xs@(x:y:z:[]) ->
                          (sum xs > 2 * x + z) &&
                          (sum xs < 2 * y + z) &&
                          (sum xs > x + 2 * y) &&
                          (sum xs < 3 * z)
property' Good        = \(x:y:z:[]) -> x + y > z
property' Bad         = \(x:y:z:[]) -> x + y <= z
property' Mix         = \(x:_:z:[]) -> x /= z

-- | classifications for a property used in testing triangle construction.
classifys :: (Testable prop) => [Int] -> prop -> Property
classifys xs@(_:_:_:[]) = classify (length xs == 3) "3-elem list input" .
                          classify (property' Bad $ xs)  "bad" .
                          if (property' Good $ xs)
                            then classify (property' Good $ xs) "triangle" .
                                 classify (property' Equilateral $ xs) "equilateral" .
                                 classify (property' Isoceles $ xs) "isoceles" .
                                 classify (property' Scalene $ xs) "scalene"
                            else classify (property' Good $ xs) "triangle"
classifys _             = error "need exactly a 3-element list."

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | run `quickcheck` on all properties automatically.

-- | test setup.
-- set up to run quickcheck using template haskell; needs template haskell extn.
-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
-- /u/ willem van onsem @ https://tinyurl.com/2p9h3csu (so)
-- https://tinyurl.com/2p9s9ets (quickcheck @ hackage)
return []

runTests :: IO Bool
runTests = $quickCheckAll

--------------------------------------------------------------------------------
-- | test runner for GHCi usage.
ghciQC :: IO ()
ghciQC = ghciRunner runTests

--------------------------------------------------------------------------------
