-- | Main module: runs all quickcheck tests in `bird-wadler` package.
-- author: Prem Muthedath, DEC 2021.
--------------------------------------------------------------------------------
import C2ReadTest

--------------------------------------------------------------------------------
-- | run all quickcheck tests in imported modules.
-- `main` acts as test runner of quickcheck tests.
-- REF: https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
--    sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
--    (<$>) :: Functor f => (a -> b) -> f a -> f b
--    and :: Foldable t => t Bool -> Bool
main :: IO ()
main = do
  good <- and <$> sequence [C2ReadTest.runTests]
  if good
     then putStrLn "=== 100% PASS ==="
     else putStrLn "=== TEST FAILURE(S) ==="
--------------------------------------------------------------------------------
