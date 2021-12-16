-- | file contains manual tests for some `Read` instances in ../src/C2Read.hs.
-- author: Prem Muthedath, DEC 2021.

-- NOTE: you can not load this file in GHCi and execute this code, as it is not 
-- in cabal path.  this file is just a placeholder for some old code.
--------------------------------------------------------------------------------

import C2Read
--------------------------------------------------------------------------------
-- | tests for `Tree`
--------------------------------------------------------------------------------
-- | run `read`, `readsPrec`, `readsPrecT` on strings to yield `Tree` values.
testTree :: IO ()
testTree = do
    putStr "1. read \"Leaf 1 :^: (Leaf 2 :^: Leaf 3)\" = "
    print (read "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: Tree Int)
    putStr "2. read \"Leaf 1\" = "
    print (read "Leaf 1" :: Tree Int)
    putStr "3. readsPrec 0 \"Leaf 1\" = "
    print (readsPrec 0 "Leaf 1" :: [(Tree Int, String)])
    putStr "4. readsPrecT 0 \"Leaf 1\" = "
    print (readsPrecT 0 "Leaf 1" :: [(Tree Int, String)])
    putStr "5. readsPrec 0 \"Leaf 1 :^: (Leaf 2 :^: Leaf 3)\" = "
    print (readsPrec 0 "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: [(Tree Int, String)])
    putStr "6. readsPrecT 0 \"Leaf 1 :^: (Leaf 2 :^: Leaf 3)\" = "
    print (readsPrecT 0 "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: [(Tree Int, String)])
    putStr "7. readsPrec 0 \"Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))\" = "
    print (readsPrec 0 "Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))" :: [(Tree Int, String)])
    putStr "8. readsPrecT 0 \"Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))\" = "
    print (readsPrecT 0 "Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))" :: [(Tree Int, String)])
    putStr "9. read \"(Leaf 1, Leaf 2 :^: Leaf 3)\" = "
    print (read "(Leaf 1, Leaf 2 :^: Leaf 3)" :: (Tree Int, Tree Int))
    putStr "10. read \"[Leaf 1, Leaf 2 :^: Leaf 3]\" = "
    print (read "[Leaf 1, Leaf 2 :^: Leaf 3]" :: [Tree Int])
    putStr "11. read \"(Leaf 1 :^: Leaf 2) :^: Leaf 3\" = "
    print (read "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: Tree Int)
    putStr "12. readsPrec 0 \"[(Leaf 1 :^: Leaf 2) :^: Leaf 3]\" = "
    print (readsPrec 0 "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: [(Tree Int, String)])
    putStr "13. readsPrecT 0 \"(Leaf 1 :^: Leaf 2) :^: Leaf 3\" = "
    print (readsPrecT 0 "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: [(Tree Int, String)])
    putStr "14. readsPrec 0 \"Leaf 1 :^: Leaf 2 :^: Leaf 3\" = "
    print (readsPrec 0 "Leaf 1 :^: Leaf 2 :^: Leaf 3" :: [(Tree Int, String)])
    putStr "15. readsPrecT 0 \"Leaf 1 :^: Leaf 2 :^: Leaf 3\" = "
    print (readsPrecT 0 "Leaf 1 :^: Leaf 2 :^: Leaf 3" :: [(Tree Int, String)])
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | tests for `SomeType`.
--------------------------------------------------------------------------------
-- | run `read`, `readsPrec`, `readsPrecST` on strings to get `SomeType` values.
testSomeType :: IO ()
testSomeType = do
  putStr "1. read \"(3 4)\" = "
  print (read "(3 4)" :: SomeType Int)
  putStr "2. read \"4\" = "
  print (read "4" :: SomeType Int)
  putStr "3. readsPrec 0 \"(3 4)\" = "
  print (readsPrec 0 "(3 4)" :: [(SomeType Int, String)])
  putStr "4. readsPrecST 0 \"(3 4)\" = "
  print (readsPrecST 0 "(3 4)" :: [(SomeType Int, String)])
  putStr "5. readsPrec 0 \"4\" = "
  print (readsPrec 0 "4" :: [(SomeType Int, String)])
  putStr "6. readsPrecST 0 \"4\" = "
  print (readsPrecST 0 "4" :: [(SomeType Int, String)])
  -- | show $ Mix (Mix (Type 4) (Mix (Type 5) (Type 6))) (Type 7)
  --    = "((4 (5 6)) 7)"
  putStr "7. read \"((4 (5 6)) 7)\" = "
  print (read "((4 (5 6)) 7)" :: SomeType Int)
  -- | show $ Mix (Mix (Type 4) (Mix (Type 5) (Type 6))) (Type 7)
  --    = "((4 (5 6)) 7)"
  putStr "8. readsPrecST \"((4 (5 6)) 7)\" = "
  print (readsPrecST 0 "((4 (5 6)) 7)" :: [(SomeType Int, String)])
  putStr "9. read \"((3 4), 5)\" = "
  print (read "((3 4), 5)" :: (SomeType Int, SomeType Int))
  putStr "10. read \"[(3 4), 5]\" = "
  print (read "[(3 4), 5]" :: [SomeType Int])
--------------------------------------------------------------------------------
