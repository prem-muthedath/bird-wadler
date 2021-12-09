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
