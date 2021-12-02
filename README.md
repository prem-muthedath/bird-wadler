#### bird-wadler

Study of "Introduction to Functional Programming", Bird & Wadler.

Author: Prem Muthedath

REF:
  1. https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
  2. QuickCheck @ hackage: https://tinyurl.com/e98m55wc

HOW TO RUN THE PROGRAM AND TESTS:
  1. `cd` to `qcheck-original`, the top directory containing this `README` file, 
     and remain there to execute any of the steps below.
  2. to load QuickCheck library into `GHCi`, type below command at the 
     commandline & press `RETURN`:
        - `cabal v2-repl :qcheck-original`

     next, enable `ScopedTypeVariables` extension in `GHCi`, using the `:set` 
     command, as shown below.
        - `:set -XScopedTypeVariables`

     you can now use the library in `GHCi`, just as you would use any library; 
     see below for examples of commands you can enter at `GHCi` prompt:
        - `quickCheck (\(x :: Int) -> x + 1 == 1 + x)`
        - `verboseCheck (\(x :: Int) -> x + 1 == 1 + x)`
        - `do; y <- generate $ evaluate (\(x :: Int) -> x + 1 == 1 + x); print 
          (ok y)`
  3. to run all tests on QuickCheck at the commandline (NOTE: if you are in 
     `GHCi`, first exit from there), type below command & press `RETURN`:
        - `cabal v2-run :qcheck-original-test`
  4. to run all tests through `GHCi`, first type below command at the 
     commandline & press `ENTER`, which will load the tests into `GHCi`:
        - `cabal v2-repl :qcheck-original-test`

      next, to run all tests in `GHCi`, type below command at `GHCi` prompt & 
      press `ENTER`:
        - `main`
  5. to run individual tests in `GHCi`, first type below command at the 
     commandline & press `ENTER`, which will load the tests into `GHCi`:
        - `cabal v2-repl :qcheck-original-test`

     you can then run any test you wish in `GHCi`; some examples:
        - `quickCheck prop_pass`
        - `quickCheck prop_fail`
        - `quickCheck prop_choose`


