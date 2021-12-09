-- | common quickcheck testing code used by 1 or more modules in `test` dir.
-- author: Prem Muthedath, DEC 2021.

--------------------------------------------------------------------------------
module QCTest where

--------------------------------------------------------------------------------
import Test.QuickCheck
--------------------------------------------------------------------------------
-- | run `QuickCheck` tests.
qc :: [(String, Property)] -> IO ()
qc tests = mapM_ qc' tests
  where -- | run `QuickCheck` test case.
        qc' :: (String, Property) -> IO ()
        qc' (x, y) = do
             putStrLn $ "\n--- " <> x <> " ---"
             quickCheck y
--------------------------------------------------------------------------------
