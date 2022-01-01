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
        -- some non-leap years generated may be divisible by 4!
        (x `mod` 4 =/= 0) .||. (x `mod` 100 =/= 0  .||.  x `mod` 400 =/= 0)

-- | property to test a leap year.
prop_leap :: Property
prop_leap = forAll genLeap $
              \x -> leap_classifys x $
                    leap' x === True

-- | property to test a leap year not divisible by 100.
prop_leap4 :: Property
prop_leap4 = forAll leap4'  $
              \x -> leap_classifys x $
                    leap' x === leap' (x + 4)
  where leap4' :: Gen Int
        leap4' = do
          y :: Int <- genYear leap4   -- NOTE: y `mod` 100 /= 0
          if (y + 4) `mod` 100 == 0
             then return $ y - 4
             else return y

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
                   classify (x `mod` 4 == 0) "divisible by 4" .
                   classify (x `mod` 100 /= 0) "has no 00" .
                   classify (x `mod` 100 == 0) "has 00" .
                   classify (x `mod` 400 == 0) "divisible by 400"

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
-- list of all possible traingles considered.
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
-- | `Sides` data type: represents sides of a triangle, good or bad.
-- `Sides x y z` represents sides `x`, `y`, `z` of a triangle in order, where 
-- all sides are + integers > 0, and x <= y <= z.
-- NOTE: this definition does NOT enforce the triangle criterion, `x + y < z`.
data Sides = Sides Int Int Int deriving (Eq, Show)
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `Sides`.
instance Arbitrary Sides where
  arbitrary = do
      xs :: [Positive Int] <- vectorOf 3 (arbitrary :: Gen (Positive Int))
      case (sort xs) of
        (x:y:z:[])  -> return $
                        Sides (getPositive x)
                              (getPositive y)
                              (getPositive z)
        _           -> error "orbitrary.Sides: only 3-element list allowed."
--------------------------------------------------------------------------------
-- | check if `Triangle` generator is valid.
prop_validTriangle :: Property
prop_validTriangle = forAll (arbitrary :: Gen Triangle) $
  \x -> x `elem` [toEnum 0 :: Triangle .. ]
--------------------------------------------------------------------------------
-- | check if `Sides` generater is valid.
prop_trian_validSides :: Property
prop_trian_validSides = forAll (assorted :: Gen Sides) $
                          \s@(Sides x y z) ->
                            let xs = [x, y, z]
                            in classifys s $
                               (sort xs === xs) .&&. (all (> 0) xs)
--------------------------------------------------------------------------------
-- | for random valid inputs, check if outputs are valid.
prop_trian_assorted :: Property
prop_trian_assorted = forAll assorted $
  \s@(Sides a b c) -> classifys s $
                      (analyze a b c) `elem` [0 .. 3]
--------------------------------------------------------------------------------
-- | for random valid inputs, outputs remain same even when inputs are doubled.
prop_trian_double :: Property
prop_trian_double = forAll assorted $
  \s@(Sides a b c) -> classifys s $
                      analyze a b c === analyze (2*a) (2*b) (2*c)
--------------------------------------------------------------------------------
-- | property to test outputs for non-existent triangle.
prop_trian_bad :: Property
prop_trian_bad = forAll (genSides Bad) $
  \s@(Sides a b c) -> classifys s $
                      analyze a b c === 0
--------------------------------------------------------------------------------
-- | property to test outputs for equilateral triangle.
prop_trian_equi :: Property
prop_trian_equi = forAll (genSides Equilateral) $
  \s@(Sides a b c) -> classifys s $
                      analyze a b c === 1
--------------------------------------------------------------------------------
-- | property to test outputs for isoceles triangle.
prop_trian_iso :: Property
prop_trian_iso = forAll (genSides Isoceles) $
  \s@(Sides a b c) -> classifys s $
                      analyze a b c === 2
--------------------------------------------------------------------------------
-- | property to test outputs for scalene triangle.
prop_trian_scal :: Property
prop_trian_scal = forAll (genSides Scalene) $
  \s@(Sides a b c) -> classifys s $
                      analyze a b c === 3
--------------------------------------------------------------------------------
-- | helper functions.

-- | generate sides for a random `Triangle`, good or bad.
assorted :: Gen Sides
assorted = do
  triangle :: Triangle <- (arbitrary :: Gen Triangle)
  genSides triangle

-- | generate sides for a `Triangle`.
genSides :: Triangle -> Gen Sides
-- NOTE: because generating `Sides` that has all identical values through random 
-- sampling is a rare event, it takes a while to generate sides of an 
-- `Equilateral` triangle like the way it is done for other triangles. so we use 
-- a different algorithm for an `Equilateral` triangle.
genSides Equilateral = do
        x <- chooseInt (1, 1000)
        return $ Sides x x x
genSides triangle    = (arbitrary :: Gen Sides) `suchThat` f
  where f :: (Sides -> Bool)
        f | triangle `elem` [Mix, Bad] = g
          | otherwise                  = \x -> g x && h x
        g :: (Sides -> Bool)
        g = property' triangle
        h :: (Sides -> Bool)
        h = property' Good

-- return property (a lambda) associated with a given `Triangle`.
-- `property'`represents rules the sides of a given `Triangle` must obey.
-- NOTE: in case of valid triangles other than `Good`, the returned property 
-- does NOT include the basic check of whether or not something is a triangle.  
-- if you need to generate a valid triangle, use `genSides` given above.
property' :: Triangle -> (Sides -> Bool)
-- we define somewhat complex & ingenious triangle properties, because we do not 
-- want to replicate the source code here, and in that way duplicate bugs!
property' Equilateral = \(Sides x y z) -> x + y + z == 3 * x
property' Isoceles    = \(Sides x y z) ->
                          let tot = x + y + z
                          in (tot /= 3 * x) &&
                             ((tot == 2 * y + z) ||
                              (tot == x + 2 * y))
property' Scalene     = \(Sides x y z) ->
                          let tot = x + y + z
                          in (tot > 2 * x + z) &&
                             (tot < 2 * y + z) &&
                             (tot > x + 2 * y) &&
                             (tot < 3 * z)
property' Good        = \(Sides x y z) -> x + y > z
property' Bad         = \(Sides x y z) -> x + y <= z
property' Mix         = \(Sides x _ z) -> x /= z

-- | classifications for a property used in testing triangle construction.
classifys :: (Testable prop) => Sides -> prop -> Property
classifys s = classify (property' Bad $ s)  "bad" .
              if (property' Good $ s)
                 then classify (property' Good $ s) "triangle" .
                      classify (property' Equilateral $ s) "equilateral" .
                      classify (property' Isoceles $ s) "isoceles" .
                      classify (property' Scalene $ s) "scalene"
                 else classify (property' Good $ s) "triangle"

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
