{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | quickcheck tests for ../src/C2Read.hs
-- author: Prem Muthedath, DEC 2021.
-- usage:
--  1. `cd` to `bird-wadler` directory, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler` to start GHCi.
--  3. at GHCi prompt, enter `import C2ReadTest`.
--  4. you can then invoke `runAllQC` to run all quickcheck tests.

--------------------------------------------------------------------------------
module C2ReadTest where

--------------------------------------------------------------------------------
import Test.QuickCheck
import Control.Monad (liftM, liftM2)

import C2Read
import QCTest
--------------------------------------------------------------------------------
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
-- | QuickCheck tests for `Tree`.
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `Tree`.
instance (Read a, Show a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized genTree

-- | helper functions for implementing arbitrary.
-- | generator for Tree type.
genTree :: forall a. (Read a, Show a, Arbitrary a) => Int -> Gen (Tree a)
genTree 0 = liftM Leaf (arbitrary :: Gen a)
genTree n = frequency [
      (1, liftM Leaf (arbitrary :: Gen a)),
      (4, liftM2 (:^:) (genTree (n `div` 2)) (genTree (n `div` 2)))
    ]

-- | valid `Tree` property.
prop_validTree :: Property
prop_validTree = forAll (arbitrary :: Gen (Tree Int)) $
  \x -> classify (isValid x) "valid tree" $
        isValid x
  where isValid :: Tree a -> Bool
        isValid (Leaf _)  = True
        isValid (l :^: r) = (isValid l) && (isValid r)

-- | quickcheck property to test `Tree` read.
prop_readTree :: (Int -> ReadS (Tree Int)) -> Property
prop_readTree f = forAll (arbitrary :: Gen (Tree Int)) $
  \x -> classify (isLeaf x) "leaf" $
        classify (depthT x > 3) "depth > 3" $
        classify (ldepthT x > 3) "left depth > 3" $
        classify (rdepthT x > 3) "right depth > 3" $
        (x,"") `elem` (f 0 (showsPrec 0 x ""))

-- | some helper functions.
isLeaf :: Tree a -> Bool
isLeaf (Leaf _) = True
isLeaf _        = False

depthT :: Tree a -> Int
depthT (Leaf _)  = 1
depthT (l :^: r) = 1 + max (depthT l) (depthT r)

ldepthT :: Tree a -> Int
ldepthT (Leaf _)  = 1
ldepthT (l :^: _) = 1 + ldepthT l

rdepthT :: Tree a -> Int
rdepthT (Leaf _)  = 1
rdepthT (_ :^: r) = 1 + rdepthT r

-- | quickcheck property to test `Tree` list read.
prop_readTreeList :: Property
prop_readTreeList = forAll (genList :: Gen [Tree Int]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         -- | NOTE: you can NOT substitute `readsPrecT` for `readsPrec` in below 
         -- line of code, as `readPrecT` returns `ReadS (Tree a)`, not `ReadS 
         -- [Tree a]`.  on the other hand, `readsPrec` returns `ReadS a`, so it 
         -- can handle `ReadS [Tree a]` as well. if you want to use `readsPrecT` 
         -- for reading lists, you have to set `readsPrec = readsPrecT` in the 
         -- `Read` instance for `Tree`.  i am not clear why such an equivalence 
         -- works, but it indeed does!
         (xs,"") `elem` (readsPrec 0 (showsPrec 0 xs ""))

-- | generate a random list of type `a`.
-- for `<$>` and `<*>`, see https://tinyurl.com/42h7z9vn (so)
-- ((:) <$> (arbitrary :: Gen a))
-- :: Gen ([a] -> [a])
-- (((:) <$> (arbitrary :: Gen a)) <*> myList)
-- :: Gen [a]
genList :: forall a. (Read a, Show a, Arbitrary a) => Gen [a]
genList = frequency
  [ (1, return [])
  , (4, ((:) <$> (arbitrary :: Gen a)) <*> genList)
  ]

-- | is list generated valid?
-- since `genList` generates a list of type `a`, it is used by all `Read` 
-- instances. so if we test it for `[Int]`, we should be just fine.
prop_validList :: Property
prop_validList = forAll (genList :: Gen [Int]) $
  \xs -> classify (length xs == 0) "empty list" $
         classify (length xs == 1) "singleton list" $
         classify (length xs > 1)  "> 1 element list" $
         isValid xs
  where isValid :: [a] -> Bool
        isValid []      = True
        isValid (_:ys)  = isValid ys

-- | quickcheck property to test `Tree` tuple read.
prop_readTreeTuple :: Property
prop_readTreeTuple = forAll (genTuple :: Gen (Tree Int, Tree Int)) $
  \(x, y) -> classify (isLeaf x) "fst is leaf" $
             classify (isLeaf y) "snd is leaf" $
             classify (depthT x > 3) "fst depth > 3" $
             classify (depthT y > 3) "snd depth > 3" $
             -- | NOTE: you can NOT substitute `readsPrecT` for `readsPrec` in 
             -- below line of code, as `readPrecT` returns `ReadS (Tree a)`, not 
             -- `ReadS (Tree a, Tree a)`.  on the other hand, `readsPrec` 
             -- returns `ReadS a`, so it can handle `ReadS (Tree a, Tree a)` as 
             -- well.  if you want to use `readsPrecT` for reading tuples, you 
             -- have to set `readsPrec = readsPrecT` in `Read` instance for 
             -- `Tree`.  i am unclear why that equivalence works!
             ((x, y),"") `elem` (readsPrec 0 (showsPrec 0 (x, y) ""))

-- | generate a random 2-tuple of type `a`.
genTuple :: forall a. (Read a, Show a, Arbitrary a) => Gen (a, a)
genTuple = do
  x1 <- arbitrary :: Gen a
  y1 <- arbitrary :: Gen a
  return (x1, y1)

-- | `Tree` QuickCheck test cases.
treeTC :: [(String, Property)]
treeTC = let f :: Int -> ReadS (Tree Int) = readsPrec
             g :: Int -> ReadS (Tree Int) = readsPrecT
         in [("valid tree", prop_validTree),
             ("readsPrec tree", prop_readTree f),
             ("readsPrecT tree", prop_readTree g),
             ("readsPrec tree list", prop_readTreeList),
             ("readsPrec tree tuple", prop_readTreeTuple)]

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
-- | QuickCheck tests for `SomeType`.
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `SomeType`.
instance (Read a, Show a, Arbitrary a) => Arbitrary (SomeType a) where
  arbitrary = sized genSomeType

-- | helper functions for implementing arbitrary.
-- | generator for SomeType type.
genSomeType :: forall a. (Read a, Show a, Arbitrary a) => Int -> Gen (SomeType a)
genSomeType 0 = liftM Type (arbitrary :: Gen a)
genSomeType n = frequency [
      (1, liftM Type (arbitrary :: Gen a)),
      (4, liftM2 Mix (genSomeType (n `div` 2)) (genSomeType (n `div` 2)))
    ]

-- | valid `SomeType` property.
prop_validSomeType :: Property
prop_validSomeType = forAll (arbitrary :: Gen (SomeType Int)) $
  \x -> classify (isValid x) "valid SomeType" $
        isValid x
  where isValid :: SomeType a -> Bool
        isValid (Type _)  = True
        isValid (Mix l r) = (isValid l) && (isValid r)

-- | quickcheck property to test `SomeType` read.
prop_readSomeType :: (Int -> ReadS (SomeType Int)) -> Property
prop_readSomeType f = forAll (arbitrary :: Gen (SomeType Int)) $
  \x -> classify (isType x) "Type" $
        classify (depthST x > 3) "depth > 3" $
        classify (ldepthST x > 3) "left depth > 3" $
        classify (rdepthST x > 3) "right depth > 3" $
        (x,"") `elem` (f 0 (showsPrec 0 x ""))

-- | some helper functions.
isType :: SomeType a -> Bool
isType (Type _) = True
isType _        = False

depthST :: SomeType a -> Int
depthST (Type _)  = 1
depthST (Mix l r) = 1 + max (depthST l) (depthST r)

ldepthST :: SomeType a -> Int
ldepthST (Type _)  = 1
ldepthST (Mix l _) = 1 + ldepthST l

rdepthST :: SomeType a -> Int
rdepthST (Type _)  = 1
rdepthST (Mix _ r) = 1 + rdepthST r

-- | quickcheck property to test `SomeType` list read.
prop_readSomeTypeList :: Property
prop_readSomeTypeList = forAll (genList :: Gen [SomeType Int]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         -- | NOTE: you can NOT substitute `readsPrecT` for `readsPrec` in below 
         -- line of code, as `readPrecT` returns `ReadS (SomeType a)`, not 
         -- `ReadS [SomeType a]`.  on the other hand, `readsPrec` returns `ReadS 
         -- a`, so it can handle `ReadS [SomeType a]` as well. if you want to 
         -- use `readsPrecT` for reading lists, you have to set `readsPrec = 
         -- readsPrecT` in the `Read` instance for `SomeType`.  i am not clear 
         -- why such an equivalence works, but it indeed does!
         (xs,"") `elem` (readsPrec 0 (showsPrec 0 xs ""))

-- | quickcheck property to test `SomeType` tuple read.
prop_readSomeTypeTuple :: Property
prop_readSomeTypeTuple = forAll (genTuple :: Gen (SomeType Int, SomeType Int)) $
  \(x, y) -> classify (isType x) "fst is leaf" $
             classify (isType y) "snd is leaf" $
             classify (depthST x > 3) "fst depth > 3" $
             classify (depthST y > 3) "snd depth > 3" $
             -- | NOTE: you can NOT substitute `readsPrecT` for `readsPrec` in 
             -- below line of code, as `readPrecT` returns `ReadS (SomeType a)`, 
             -- not `ReadS (SomeType a, SomeType a)`.  on the other hand, 
             -- `readsPrec` returns `ReadS a`, so it can handle `ReadS (SomeType 
             -- a, SomeType a)` as well.  if you want to use `readsPrecT` for 
             -- reading tuples, you have to set `readsPrec = readsPrecT` in 
             -- `Read` instance for `Tree`. unclear why that equivalence works!
             ((x, y),"") `elem` (readsPrec 0 (showsPrec 0 (x, y) ""))

-- | `SomeType` QuickCheck test cases.
someTypeTC :: [(String, Property)]
someTypeTC = let f :: Int -> ReadS (SomeType Int) = readsPrec
                 g :: Int -> ReadS (SomeType Int) = readsPrecST
             in [("valid SomeType", prop_validSomeType),
                 ("readsPrec SomeType", prop_readSomeType f),
                 ("readsPrecST SomeType", prop_readSomeType g),
                 ("readsPrec SomeType list", prop_readSomeTypeList),
                 ("readsPrec SomeType tuple", prop_readSomeTypeTuple)]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | run `QuickCheck` tests on all `Read` instances.
runAllQC :: IO ()
runAllQC = qc tests
  where tests :: [(String, Property)]
        tests = [("valid list", prop_validList)] ++ treeTC ++ someTypeTC

--------------------------------------------------------------------------------
