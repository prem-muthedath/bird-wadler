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
import Data.List (isInfixOf)
import Data.Char (isDigit)

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
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
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

-- | quickCheck property to test `Tree` read for random non-Tree strings.
prop_readNonTreeStr :: (Int -> ReadS (Tree Int)) -> Property
prop_readNonTreeStr f = forAll (nonTree :: Gen String) $
  \x -> classify (x == "") "empty string" $
        classify (length x == 1) "1-elem str" $
        classify (length x > 1) "> 1 elem string" $
        f 0 x === []
  where nonTree :: Gen String
        nonTree = (arbitrary :: Gen String)
                  `suchThat`
                  (\x -> not ("Leaf" `isInfixOf` x))

-- | quickcheck property to test `Tree` list read.
prop_readTreeList :: Property
prop_readTreeList = forAll (genList :: Gen [Tree Int]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         -- | NOTE: you can NOT substitute `readsPrecT` for `readsPrec` in below 
         -- line of code, as `readsPrecT` returns `ReadS (Tree a)`, not `ReadS 
         -- [Tree a]`.  on the other hand, `readsPrec` returns `ReadS a`, so it 
         -- can handle `ReadS [Tree a]` as well. if you want to use `readsPrecT` 
         -- for reading lists, you have to set `readsPrec = readsPrecT` in the 
         -- `Read` instance for `Tree`.  i am not clear why such an equivalence 
         -- works, but it indeed does!
         (xs,"") `elem` (readsPrec 0 (showsPrec 0 xs ""))

-- | generate a random list of type `a`.
-- for `<$>` and `<*>`, see https://tinyurl.com/42h7z9vn (so)
-- from Control.Monad, we have the following type signatures:
--    (<$>) :: Functor f => (a -> b) -> f a -> f b
--    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
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
             -- below line of code, as `readsPrecT` returns `ReadS (Tree a)`, 
             -- not `ReadS (Tree a, Tree a)`.  on the other hand, `readsPrec` 
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
             ("readsPrec non-tree string", prop_readNonTreeStr f),
             ("readsPrecT non-tree string", prop_readNonTreeStr g),
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
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
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

-- | property to test `SomeType` read for random non-SomeType strings.
prop_readNonSomeTypeStr :: (Int -> ReadS (SomeType Int)) -> Property
prop_readNonSomeTypeStr f = forAll (notSomeType :: Gen String) $
  \x -> classify (x == "") "empty string" $
        classify (length x == 1) "1 character string" $
        classify (length x > 1) "string has > 1 character" $
        f 0 x === []
  where notSomeType :: Gen String
        notSomeType = (arbitrary :: Gen String)
                      `suchThat`
                      (\x -> all (not . isDigit) x)

-- | quickcheck property to test `SomeType` list read.
prop_readSomeTypeList :: Property
prop_readSomeTypeList = forAll (genList :: Gen [SomeType Int]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         -- | NOTE: you can NOT substitute `readsPrecT` for `readsPrec` in below 
         -- line of code, as `readsPrecT` returns `ReadS (SomeType a)`, not 
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
             -- below line of code, as `readsPrecT` returns `ReadS (SomeType 
             -- a)`, not `ReadS (SomeType a, SomeType a)`.  on the other hand, 
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
                 ("readsPrec non-SomeType string", prop_readNonSomeTypeStr f),
                 ("readsPrecST non-SomeType string", prop_readNonSomeTypeStr g),
                 ("readsPrec SomeType list", prop_readSomeTypeList),
                 ("readsPrec SomeType tuple", prop_readSomeTypeTuple)]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | QuickCheck tests for `TT` read.
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `TT`.
instance Arbitrary (TT) where
  arbitrary = sized genTT

-- | helper functions for implementing arbitrary.
-- | generator for `TT` type.
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
genTT :: Int -> Gen TT
genTT 0 = return NT
genTT n = frequency [
        (1, return NT),
        (4, liftM2 (:$) (arbitrary :: Gen Int) (genTT (n `div` 2)))
      ]

-- | valid `TT` property.
prop_validTT :: Property
prop_validTT = forAll (arbitrary :: Gen TT) $
  \x -> classify (isValid x) "valid TT" $
        isValid x
  where isValid :: TT -> Bool
        isValid (NT)  = True
        isValid (x :$ y) =  (x < 0 || x >= 0) && (isValid y)

-- | property to test `TT` read.
prop_readTT :: Property
prop_readTT = forAll (arbitrary :: Gen TT) $
  \x -> classify (isNT x) "NT" $
        classify (depthTT x == 1) "depthTT = 1" $
        classify (depthTT x > 3) "depthTT > 3" $
        (x, "") `elem` (readsPrec 0 (showsPrec 0 x ""))

-- | helper functions
isNT :: TT -> Bool
isNT (NT) = True
isNT _    = False

depthTT :: TT -> Int
depthTT (NT) = 1
depthTT (_ :$ y) = 1 + depthTT y

-- | property to test `TT` read of random non-TT string.
prop_readNonTTStr :: Property
prop_readNonTTStr = forAll (notTT :: Gen String) $
  \x -> classify (x == "") "empty string" $
        classify (length x == 1) "1-elem string" $
        classify (length x > 1) "> 1 elem string" $
        (readsPrec 0 x :: [(TT, String)]) == []
  where notTT :: Gen String
        notTT = (arbitrary :: Gen String)
                `suchThat`
                (\x -> not ("NT" `isInfixOf` x))

-- | quickcheck property to test `TT` list read.
prop_readTTList :: Property
prop_readTTList = forAll (genList :: Gen [TT]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         (xs,"") `elem` (readsPrec 0 (showsPrec 0 xs ""))

-- | quickcheck property to test `TT` tuple read.
prop_readTTTuple :: Property
prop_readTTTuple = forAll (genTuple :: Gen (TT, TT)) $
  \(x, y) -> classify (isNT x) "fst is NT" $
             classify (isNT y) "snd is NT" $
             classify (depthTT x > 3) "fst depth > 3" $
             classify (depthTT y > 3) "snd depth > 3" $
             ((x, y),"") `elem` (readsPrec 0 (showsPrec 0 (x, y) ""))

-- | `TT` QuickCheck test cases.
ttTC :: [(String, Property)]
ttTC = [("valid TT", prop_validTT),
        ("readsPrec TT", prop_readTT),
        ("readsPrec non-TT string", prop_readNonTTStr),
        ("readsPrec TT list", prop_readTTList),
        ("readsPrec TT tuple", prop_readTTTuple)
      ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | QuickCheck tests for `P` & `T` read.
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `P`.
instance Arbitrary (P) where
  arbitrary = return P

-- | `Arbitrary` instance for `T`.
instance Arbitrary (T) where
  arbitrary = genT

-- | helper functions for implementing arbitrary.
-- | generator for `T` type.
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
genT :: Gen T
genT = frequency [
        (1, liftM T (arbitrary :: Gen P)),
        (4, liftM2 (:#) (arbitrary :: Gen P) (arbitrary :: Gen P))
      ]

-- | valid `T` property.
-- this also tests in an indirect way if generator of `P` is valid.
prop_validT :: Property
prop_validT = forAll (arbitrary :: Gen T) $
  \x -> classify (isValid x) "valid T" $
        isValid x
  where isValid :: T -> Bool
        isValid (T P)  = True
        isValid (x :# y) =  (x == P) && (y == P)

-- | property to test `P` read.
prop_readP :: Property
prop_readP = forAll (arbitrary :: Gen P) $
  \x -> (x, "") `elem` (readsPrec 0 (showsPrec 0 x ""))

-- | property to test `T` read.
prop_readT :: Property
prop_readT = forAll (arbitrary :: Gen T) $
  \x -> classify (isTP x) "T P" $
        (x, "") `elem` (readsPrec 0 (showsPrec 0 x ""))

-- | helper functions
isTP :: T -> Bool
isTP (T P) = True
isTP _     = False

-- | property to test `P` read of random non-P string.
prop_readNonPStr :: Property
prop_readNonPStr = forAll (notP :: Gen String) $
  \x -> classify (x == "") "empty string" $
        classify (length x == 1) "1-elem string" $
        classify (length x > 1) "> 1 elem string" $
        (readsPrec 0 x :: [(P, String)]) == []
  where notP :: Gen String
        notP = frequency [
            (1, liftM (\x  -> show x) (chooseInt (12, 1000))),
            (4, (arbitrary :: Gen String)
                `suchThat`
                (\x -> all (not . isDigit) x))
          ]

-- | property to test `T` read of random non-T string.
prop_readNonTStr :: Property
prop_readNonTStr = forAll (notT :: Gen String) $
  \x -> classify ("T " `isInfixOf` x) "T P" $
        classify (x == "") "empty string" $
        classify (length x == 1) "1-elem string" $
        classify (length x > 1) "> 1 elem string" $
        (readsPrec 0 x :: [(T, String)]) == []
  where notT :: Gen String
        notT = frequency [
            (1, liftM (\x  -> "T " <> show x) (chooseInt (12, 1000))),
            (4, (arbitrary :: Gen String)
                `suchThat`
                (\x -> all (not . isDigit) x))
          ]

-- | quickcheck property to test `P` list read.
prop_readPList :: Property
prop_readPList = forAll (genList :: Gen [P]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         (xs,"") `elem` (readsPrec 0 (showsPrec 0 xs ""))

-- | quickcheck property to test `T` list read.
prop_readTList :: Property
prop_readTList = forAll (genList :: Gen [T]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         (xs,"") `elem` (readsPrec 0 (showsPrec 0 xs ""))

-- | quickcheck property to test `P` tuple read.
prop_readPTuple :: Property
prop_readPTuple = forAll (genTuple :: Gen (P, P)) $
  \(x, y) -> ((x, y),"") `elem` (readsPrec 0 (showsPrec 0 (x, y) ""))

-- | quickcheck property to test `T` tuple read.
prop_readTTuple :: Property
prop_readTTuple = forAll (genTuple :: Gen (T, T)) $
  \(x, y) -> classify (isTP x) "fst is T P" $
             classify (isTP y) "snd is T P" $
             ((x, y),"") `elem` (readsPrec 0 (showsPrec 0 (x, y) ""))

-- | `T` & `P` QuickCheck test cases.
tpTC :: [(String, Property)]
tpTC = [("valid T", prop_validT),
        ("readsPrec P", prop_readP),
        ("readsPrec T", prop_readT),
        ("readsPrec non-P string", prop_readNonPStr),
        ("readsPrec non-T string", prop_readNonTStr),
        ("readsPrec P list", prop_readPList),
        ("readsPrec T list", prop_readTList),
        ("readsPrec P tuple", prop_readPTuple),
        ("readsPrec T tuple", prop_readTTuple)
      ]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | run `QuickCheck` tests on all `Read` instances.
runAllQC :: IO ()
runAllQC = qc tests
  where tests :: [(String, Property)]
        tests = [("valid list", prop_validList)]
                ++ treeTC
                ++ someTypeTC
                ++ ttTC
                ++ tpTC

--------------------------------------------------------------------------------
