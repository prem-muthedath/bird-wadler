{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

-- | quickcheck tests for ../src/C2Binary.hs
-- author: Prem Muthedath, JAN 2022.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler-test` to start GHCi.
--  3. at GHCi prompt, enter `import C2BinaryTest`.
--  4. you can then invoke `C2BinaryTest.ghciQC` to run all quickcheck tests.
--
--  REF: "A Guide to Writing Properties of Pure Functions", John Hughes.
--  https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf
--------------------------------------------------------------------------------
module C2BinaryTest where
--------------------------------------------------------------------------------
-- Test.QuickCheck @ https://tinyurl.com/2p9s9ets
-- Numeric (base 4.14) @ https://tinyurl.com/3kvs3fha
-- Data.Char @ https://tinyurl.com/2c72x8ya
import Test.QuickCheck
import Numeric (readInt)
import Data.Char (digitToInt)

import C2Binary
import Common (ghciRunner)
--------------------------------------------------------------------------------
-- | quickcheck testing -- binary stuff.
--------------------------------------------------------------------------------
-- | generators.
--------------------------------------------------------------------------------
-- | generate `Int` string, with generated values in range 0 .. 4294967295.
-- NOTE: as documentation, the 1st version (very crude!) listed below.
--    genIntStr = do
--      x <- (arbitrary :: Gen Char)
--           `suchThat`
--           (\x -> x `elem` digits && x /= '0')
--      xs <- listOf ((arbitrary :: Gen Char)
--                    `suchThat`
--                    (`elem` digits))
--            `suchThat`
--            (\ys -> length ys < 9)
--      return (x:xs)
--      where digits :: String = "0123456789"
genIntStr :: Gen String
genIntStr = do
  -- NOTE: 4294967295 = 2 ^ (32 :: Int) - 1
  num1 :: Int <- elements [0, 4294967295]
  num2 :: Int <- (arbitrary :: Gen Int)
                 `suchThat`
                 (\x -> x > 0 && x < 4294967295)
  frequency [ (1, return $ show num1)
            , (20, return $ show num2)
            ]
--------------------------------------------------------------------------------
-- | test generators!
--------------------------------------------------------------------------------
-- | check if generated `Int` string is valid.
prop_validIntStr :: Property
prop_validIntStr = forAll genIntStr $
  \x -> (x =/= "") .&&. (f x >= 0 .&&. f x <= 4294967295)
  where f :: String -> Int
        f = read
--------------------------------------------------------------------------------
-- | properties.
--------------------------------------------------------------------------------
-- | check `Int` string -> "binary` string.
prop_intStrToBinStr :: Property
prop_intStrToBinStr = forAll genIntStr $
  \x -> classify (x == "0") "0" $
        classify (x == "4294967295") "2^32 - 1" $
        case intStrToBinStr x of
            Left _    -> property False
            Right bin -> check x bin
  where check :: String -> String -> Property
        check x bin = r1 === r2
          where r1 :: [(Int, String)] = readsPrec 0 x
                -- readInt :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
                r2 :: [(Int, String)] = readInt 2 f g bin
                f  :: (Char -> Bool)  = (`elem` ['0', '1'])
                g  :: (Char -> Int)   = Data.Char.digitToInt
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
