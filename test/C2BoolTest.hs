{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | quickcheck tests for ../src/C2Bool.hs
-- author: Prem Muthedath, DEC 2021.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler-test` to start GHCi.
--  3. at GHCi prompt, enter `import C2BoolTest`.
--  4. you can then invoke `C2BoolTest.ghciQC` to run all quickcheck tests.
--
--  REF: "A Guide to Writing Properties of Pure Functions", John Hughes.
--  https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf

--------------------------------------------------------------------------------
module C2BoolTest where
--------------------------------------------------------------------------------
-- Test.QuickCheck @ https://tinyurl.com/2p9s9ets
-- Data.List @ https://tinyurl.com/ycxb9uaw
import Test.QuickCheck
import Data.List (sort)

import C2Bool
import Common (ghciRunner)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | quickcheck testing -- leap year stuff.
--------------------------------------------------------------------------------
-- | leap -- generators.
--------------------------------------------------------------------------------
-- | generate leap year that is not a multiple of 100.
genLeap4 :: Gen Int
genLeap4 = elements [ 4*x | x <- [1 .. 1000], 4*x `mod` 100 /= 0 ]

-- | generate leap year that is a multiple of 100.
genLeap400 :: Gen Int
genLeap400 = elements [ 400*x | x <- [1 .. 1000] ]

-- | generate leap years that include multiples & non-multiples of 100.
genLeap :: Gen Int
genLeap = frequency
  [ (1, genLeap4)
  , (1, genLeap400)
  ]

-- | generate a non-leap year.
genNonLeap :: Gen Int
genNonLeap = do
  let a = [ x | x <- [100, 200 .. 3000], x `mod` 400 /= 0 ]
      b = [ y | y <- [1 .. 100], y `mod` 4 /= 0 ]
  elements $ (a ++) . (b ++) $ []

-- | generate years, a mix of leap & non-leap.
genYears :: Gen Int
genYears = frequency
  [ (1, genLeap)
  , (1, genNonLeap)
  ]
--------------------------------------------------------------------------------
-- | leap -- properties
--------------------------------------------------------------------------------
-- | check if leap year generator is valid.
prop_validLeap :: Property
prop_validLeap = forAll genLeap $
  \x -> leap_classifys x $
        -- x `mod` 4 === 0 works because `genLeap4` has no 100 multiples; else, 
        -- we would need '(x `mod` 100 =/= 0 .&&. x `mod` 4 === 0)' instead.
        x `mod` 400 === 0 .||. x `mod` 4 === 0

-- | check if non-leap year generator is valid.
prop_validNonLeap :: Property
prop_validNonLeap = forAll genNonLeap $
  \x -> leap_classifys x $
        -- some non-leap years, say 100, generated may be divisible by 4!
        -- 100 `mod` 4 = 0, but 100 `mod` 400 /= 0, so 100 is a non-leap year.
        x `mod` 4 =/= 0 .||. x `mod` 400 =/= 0

-- | property to test equivalence of `leap` & `leap'` functions.
prop_leap_equiv :: Property
prop_leap_equiv = forAll genYears $
  \x -> leap_classifys x $
        leap x === leap' x

-- | property to test leap year.
prop_leap :: Property
prop_leap = forAll genLeap $
  \x -> leap_classifys x $
        leap' x === True

-- | property to test non-100-multiple leap year extension.
-- this is a "metamorphic" property: how does changing the input affect output?
prop_leap4_extension :: Property
prop_leap4_extension = forAll extendableLeap $
  \x -> leap_classifys x $
        (leap' x === leap' (x + 4)) .&&. (leap' x =/= leap' (x + 1))
  where extendableLeap :: Gen Int
        extendableLeap = do
          y :: Int <- genLeap4
          if (y + 4) `mod` 100 == 0
             then return $ y - 4
             else return y

-- | property to test 100-multiple leap year extension.
-- this is a "metamorphic" property: how does changing the input affect output?
prop_leap400_extension :: Property
prop_leap400_extension = forAll genLeap400 $
  \x -> leap_classifys x $
        (leap' x === leap' (x + 400)) .&&. (leap' x =/= leap'(x + 100))

-- | property to test non-leap year.
prop_non_leap :: Property
prop_non_leap = forAll genNonLeap $
  \x -> leap_classifys x $
        leap' x === False

-- | property to test non-leap year extension.
-- this is a "metamorphic" property: how does changing the input affect output?
prop_non_leap_extension :: Property
prop_non_leap_extension = forAll genNonLeap $
  \x -> leap_classifys x $
        -- extend such that the extended year is always an odd number.
        if even x
          then leap' x === leap' (x + 3)
          else leap' x === leap' (x + 2)

-- | property to test expected `leap'` function failure when passed values <= 0.
-- REF: /u/ jtobin @ https://tinyurl.com/3ndhasr7 (so)
prop_leap'_failure:: Property
prop_leap'_failure = expectFailure $
  leap' <$> ((arbitrary :: Gen Int)
             `suchThat` (\x -> x <= 0))

-- | property to test expected `leap` function failure when passed values <= 0.
-- REF: /u/ jtobin @ https://tinyurl.com/3ndhasr7 (so)
prop_leap_failure:: Property
prop_leap_failure = expectFailure $
  leap <$> ((arbitrary :: Gen Int)
            `suchThat` (<= 0))
--------------------------------------------------------------------------------
-- | classification of property used in leap year testing.
leap_classifys :: (Testable prop) => Int -> prop -> Property
leap_classifys x = classify (x `mod` 4 == 0) "divisible by 4" .
                   classify (x `mod` 4 /= 0) "not divisible by 4" .
                   classify (x `mod` 100 /= 0) "has no 00" .
                   classify (x `mod` 100 == 0) "has 00" .
                   classify (x `mod` 400 == 0) "divisible by 400" .
                   classify (x `mod` 400 /= 0) "not divisible by 400"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | quickcheck testing -- triangle stuff
--------------------------------------------------------------------------------
-- | `Triangle` data type.
-- list of all possible triangles considered.
data Triangle = Equilateral
                | Isoceles
                | Scalene
                | Bad    -- 3 sides do NOT form a triangle.
                deriving (Show, Eq, Enum)
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `Triangle`.
instance Arbitrary (Triangle) where
  arbitrary = elements [toEnum 0 :: Triangle ..]
--------------------------------------------------------------------------------
-- | `Sides` data type: represents sides of a triangle, good or bad.
-- `Sides a b c` represents sides `a`, `b`, `c` of a triangle in order, where 
-- all sides are integers > 0, and a <= b <= c.
-- NOTE: this definition does NOT enforce the triangle criterion, `a + b > c`.
type A = Int; type B = Int; type C = Int
data Sides = Sides A B C deriving (Show)
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
        _           -> error "C2BoolTest.arbitrary.Sides: only 3-element list allowed."
--------------------------------------------------------------------------------
-- | check if `Triangle` generator is valid.
prop_validTriangle :: Property
prop_validTriangle = forAll (arbitrary :: Gen Triangle) $
  \x -> x `elem` [toEnum 0 :: Triangle ..]
--------------------------------------------------------------------------------
-- | check if `Sides` generator is valid.
prop_trian_validSides :: Property
prop_trian_validSides = forAll (assorted :: Gen Sides) $
  \s@(Sides a b c) ->
    let xs = [a, b, c]
    in classifys s $
       (sort xs === xs) .&&. (all (> 0) xs)
--------------------------------------------------------------------------------
-- | for random valid inputs, check if outputs are in expected range.
prop_trian_range :: Property
prop_trian_range = forAll assorted $
  \s@(Sides a b c) -> classifys s $
                      (analyze a b c) `elem` [0 .. 3]
--------------------------------------------------------------------------------
-- | for random valid inputs, do outputs remain same when inputs are doubled?
-- this is a "metamorphic" property: how does changing the input affect output?
prop_trian_double :: Property
prop_trian_double = forAll assorted $
  \s@(Sides a b c) -> classifys s $
                      analyze a b c === analyze (2*a) (2*b) (2*c)
--------------------------------------------------------------------------------
-- | check if outputs CHANGE when inputs transform from one triangle to another.
-- another view: does each `Triangle` value produce a distinct output?
-- this is a "metamorphic" property: how does changing the input affect output?
prop_trian_transform :: Property
prop_trian_transform = forAll (arbitrary :: Gen Triangle) $
  \x -> do (Sides a b c) <- genSides x
           let original     = analyze a b c
               equilateral  = analyze a a a
               isoceles     = analyze a (a+1) (a+1)
               scalene      = analyze (a + 1) (b + 2) (c + 3)
               bad          = analyze a b (c + a + b)
           return $ classifys (Sides a b c) $ case x of
              Equilateral -> original =/= isoceles
              Isoceles    -> (original =/= scalene) .&&. (original =/= bad)
              Scalene     -> (original =/= equilateral) .&&. (original =/= bad)
              Bad         -> original =/= equilateral
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
-- | property to test expected failure.
-- REF: /u/ jtobin @ https://tinyurl.com/3ndhasr7 (so)
-- NOTE: the use of `forAll` allows display of the actual test case that failed.
-- also, this version, using function `g` that returns `Bool` instead of 
-- `Property`, avoids the 'Exception thrown while showing test case' message.
prop_trian_failure :: Property
prop_trian_failure = expectFailure $ forAll f g
  where f :: Gen (Int, Int, Int)
        f = do
          a :: Int <- chooseInt (-100, 100)
          b :: Int <- chooseInt (-100, 100)
          c :: Int <- chooseInt (-100, 100)
          let (x:y:z:[]) :: [Int] = reverse $ sort [a, b, c]
          return (x, y, z)
        g :: (Int, Int, Int) -> Bool
        g (x, y, z) = analyze x y z == 0
--------------------------------------------------------------------------------
-- | helper functions.
--------------------------------------------------------------------------------
-- | generate sides for a random `Triangle`, good or bad.
assorted :: Gen Sides
assorted = do
  triangle :: Triangle <- (arbitrary :: Gen Triangle)
  genSides triangle
--------------------------------------------------------------------------------
-- | generate sides for a `Triangle`.
genSides :: Triangle -> Gen Sides
-- NOTE: because generating `Sides` having all identical values through random 
-- sampling is a rare event, it takes a while to generate sides of an 
-- `Equilateral` triangle like the way it is done for other triangles. so we use 
-- a different algorithm for `Equilateral` triangle.
genSides Equilateral = do
        x :: Int <- chooseInt (1, 1000)
        return $ Sides x x x
genSides triangle = (arbitrary :: Gen Sides) `suchThat` f
  where f :: (Sides -> Bool)
        f | triangle == Bad = g
          | otherwise       = \s -> g s && h s
        g :: (Sides -> Bool)
        g = property' triangle
        h :: (Sides -> Bool)
        h = \(Sides a b c) -> a + b > c
--------------------------------------------------------------------------------
-- return property (a lambda) associated with a given `Triangle`.
-- `property'` represents rules the sides of a given `Triangle` must obey.
-- NOTE: in case of valid triangles, the returned property does NOT include the 
-- basic check of whether or not something is a triangle.  if you need to 
-- generate a valid triangle, use `genSides` given above.
property' :: Triangle -> (Sides -> Bool)
-- we define somewhat complex & ingenious triangle properties, because we do not 
-- want to replicate the source code here, and in that way duplicate bugs!
property' Equilateral = \(Sides a b c) -> a + b + c == 3 * a
property' Isoceles    = \(Sides a b c) ->
                          let tot = a + b + c
                          in (tot /= 3 * a) &&
                             ((tot == 2 * b + c) ||
                              (tot == a + 2 * b))
property' Scalene     = \(Sides a b c) ->
                          let tot = a + b + c
                          in (tot > 2 * a + c) &&
                             (tot < 2 * b + c) &&
                             (tot > a + 2 * b) &&
                             (tot < 3 * c)
property' Bad         = \(Sides a b c) -> a + b <= c
--------------------------------------------------------------------------------
-- | classifications for a property used in testing triangle construction.
classifys :: (Testable prop) => Sides -> prop -> Property
classifys s = if (property' Bad $ s)
                  then classify (property' Bad $ s)  "bad"
                  else  classify (property' Equilateral $ s) "equilateral" .
                        classify (property' Isoceles $ s) "isoceles" .
                        classify (property' Scalene $ s) "scalene"

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
