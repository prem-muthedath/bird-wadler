-- | contains common testing code used by 1 or more modules in `test` dir.
-- author: Prem Muthedath, DEC 2021.

--------------------------------------------------------------------------------
module Common where
--------------------------------------------------------------------------------
import System.Exit (exitSuccess, exitFailure)
--------------------------------------------------------------------------------
-- | test runner for GHCi usage.
-- run tests and exit with the appropriate unix status code.
-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
--    sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
--    (<$>) :: Functor f => (a -> b) -> f a -> f b
--    and :: Foldable t => t Bool -> Bool
-- NOTE: you can see the `EXITSUCCESS` or `EXITFAILURE` message only in GHCi, so 
-- this function is really for GHCi use.
ghciRunner :: IO Bool -> IO ()
ghciRunner tests = do
  good <- and <$> sequence [tests]
  if good
     then exitSuccess
     else exitFailure
--------------------------------------------------------------------------------
