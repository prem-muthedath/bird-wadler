{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | quickcheck tests for ../src/C2Read.hs
-- author: Prem Muthedath, DEC 2021.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler-test` to start GHCi.
--  3. at GHCi prompt, enter `import C2ReadTest`.
--  4. you can then invoke `C2ReadTest.ghciQC` to run all quickcheck tests.

-- NOTE: for old manual tests, see ../notes/chap2-Read-manual-tests.lhs
--------------------------------------------------------------------------------
module C2ReadTest where
--------------------------------------------------------------------------------
-- Test.QuickCheck @ https://tinyurl.com/2p9s9ets
-- Control.Monad @ https://tinyurl.com/mtvj95yx
-- Data.List @ https://tinyurl.com/ycxb9uaw
-- Data.Char @ https://tinyurl.com/2c72x8ya
import Test.QuickCheck
import Control.Monad (liftM, liftM2)
import Data.List (isInfixOf)
import Data.Char (isDigit)

import C2Read
import Common (ghciRunner)
--------------------------------------------------------------------------------
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
genTree n = frequency
      [ (1, liftM Leaf (arbitrary :: Gen a))
      , (4, liftM2 (:^:) (genTree (n `div` 2)) (genTree (n `div` 2)))
      ]

-- | valid `Tree` property.
prop_validTree :: Property
prop_validTree = forAll (arbitrary :: Gen (Tree Int)) $
  \x -> classify (isValid x) "valid tree" $
        isValid x
  where isValid :: Tree a -> Bool
        isValid (Leaf _)  = True
        isValid (l :^: r) = (isValid l) && (isValid r)

-- | `ReadTree` specifies a `readsPrec` implementation for `Tree`.
-- 1st parameter is name of the function; the 2nd, the function itself.
-- for `Tree`, there are 2 implementations: `readsPrec` & `readsPrecT`.
newtype ReadTree = ReadTree (String, Int -> ReadS (Tree Int))

-- | `Show` instance for `ReadTree`.
--  1. `quickCheck` needs a `Show` instance for `ReadTree` because of how 
--     `Testable` instance for functions are defined (see below).
--          source: https://www.fpcomplete.com/blog/2017/01/quickcheck/
--          instance [safe] (Arbitrary a, Show a, Testable prop)
--            => Testable (a -> prop)
--  2. quickcheck can test a function of any number of arguments as long as each 
--     one of the arguments is an instance of `Arbitrary` & `Show`. by being an 
--     instance of `Show`, if a test fails, offending value can be displayed.
--  3. in haskell, we can NOT show functions, so we just show function's name.
instance Show (ReadTree) where
  show (ReadTree (name, _)) = name

-- | `Arbitrary` instance for `ReadTree`.
-- we randomly choose from `readsPrec` & `readsPrecT` defined for `Tree`.
instance Arbitrary (ReadTree) where
  arbitrary = let f :: Int -> ReadS (Tree Int) = readsPrec
                  g :: Int -> ReadS (Tree Int) = readsPrecT
              in frequency [
                  (1, return $ ReadTree ("readsPrec", f)),
                  (1, return $ ReadTree ("readsPrecT", g))
                ]

-- | quickcheck property to test `Tree` read.
prop_readTree :: ReadTree -> Property
prop_readTree (ReadTree (name, f)) = forAll (arbitrary :: Gen (Tree Int)) $
  \x -> classify (name == "readsPrec") "readsPrec tree" $
        classify (name == "readsPrecT") "readsPrecT tree" $
        classify (isLeaf x) "leaf" $
        classify (depthT x > 3) "depth > 3" $
        classify (ldepthT x > 3) "left depth > 3" $
        classify (rdepthT x > 3) "right depth > 3" $
        checkRead f x

-- | checks if `readsPrec` (or its equivalent) reads what `showsPrec` outputs.
-- the check is made for a random selection of precedence level. the check holds 
-- true only if both `readsPrec` & `showsPrec` are given the same precdence.
checkRead :: forall a. (Eq a, Show a, Read a, Arbitrary a)
          => (Int -> ReadS a)   -- `readsPrec` or its equivalent
          -> a                  -- input to `showsPrec`
          -> Property
checkRead f x = forAll (elements [0 .. 11]) $
                  \d -> label ("test precedence: " ++ show d) $
                        (x,"") `elem` (f d (showsPrec d x ""))

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
prop_readNonTreeStr :: ReadTree -> Property
prop_readNonTreeStr (ReadTree (name, f)) = forAll (nonTree :: Gen String) $
  \x -> classify (name == "readsPrec") "readsPrec tree" $
        classify (name == "readsPrecT") "readsPrecT tree" $
        classify (x == "") "empty string" $
        classify (length x == 1) "1-elem str" $
        classify (length x > 1) "> 1 elem string" $
        f 0 x === []
  where nonTree :: Gen String
        nonTree = (arbitrary :: Gen String)
                  `suchThat`
                  (\x -> not ("Leaf" `isInfixOf` x))
        -- use this negative test, which should fail, to test this prop.
        -- nonTree = elements ["Leaf 0", "Leaf 1"]

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
         checkRead readsPrec xs

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
             checkRead readsPrec (x, y)

-- | generate a random 2-tuple of type `a`.
genTuple :: forall a. (Read a, Show a, Arbitrary a) => Gen (a, a)
genTuple = do
  x1 <- arbitrary :: Gen a
  y1 <- arbitrary :: Gen a
  return (x1, y1)

--------------------------------------------------------------------------------
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

-- | `ReadST` specifies a `readsPrec` implementation for `SomeType`.
-- 1st parameter is name of the function; the 2nd, the function itself.
-- for `SomeType`, there are 2 implementations: `readsPrec` & `readsPrecST`.
newtype ReadST = ReadST (String, Int -> ReadS (SomeType Int))

-- | `Show` instance for `ReadST`.
--  1. `quickCheck` needs a `Show` instance for `ReadST` because of how 
--     `Testable` instance for functions are defined (see below).
--          source: https://www.fpcomplete.com/blog/2017/01/quickcheck/
--          instance [safe] (Arbitrary a, Show a, Testable prop)
--            => Testable (a -> prop)
--  2. quickcheck can test a function of any number of arguments as long as each 
--     one of the arguments is an instance of `Arbitrary` & `Show`. by being an 
--     instance of `Show`, if a test fails, offending value can be displayed.
--  3. in haskell, we can NOT show functions, so we just show function's name.
instance Show (ReadST) where
  show (ReadST (name, _)) = name

-- | `Arbitrary` instance for `ReadST`.
-- we randomly choose from `readsPrec` & `readsPrecST` defined for `SomeType`.
instance Arbitrary (ReadST) where
  arbitrary = let f :: Int -> ReadS (SomeType Int) = readsPrec
                  g :: Int -> ReadS (SomeType Int) = readsPrecST
              in frequency [
                  (1, return $ ReadST ("readsPrec", f)),
                  (1, return $ ReadST ("readsPrecST", g))
                ]

-- | quickcheck property to test `SomeType` read.
prop_readSomeType :: ReadST -> Property
prop_readSomeType (ReadST (name, f)) = forAll (arbitrary :: Gen (SomeType Int)) $
  \x -> classify (name == "readsPrec") "readsPrec SomeType" $
        classify (name == "readsPrecST") "readsPrecST SomeType" $
        classify (isType x) "Type" $
        classify (depthST x > 3) "depth > 3" $
        classify (ldepthST x > 3) "left depth > 3" $
        classify (rdepthST x > 3) "right depth > 3" $
        checkRead f x

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
prop_readNonSomeTypeStr :: ReadST -> Property
prop_readNonSomeTypeStr (ReadST (name, f)) = forAll (notSomeType :: Gen String) $
  \x -> classify (name == "readsPrec") "readsPrec SomeType" $
        classify (name == "readsPrecST") "readsPrecST SomeType" $
        classify (x == "") "empty string" $
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
         checkRead readsPrec xs

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
             checkRead readsPrec (x, y)

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
        checkRead readsPrec x

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
         checkRead readsPrec xs

-- | quickcheck property to test `TT` tuple read.
prop_readTTTuple :: Property
prop_readTTTuple = forAll (genTuple :: Gen (TT, TT)) $
  \(x, y) -> classify (isNT x) "fst is NT" $
             classify (isNT y) "snd is NT" $
             classify (depthTT x > 3) "fst depth > 3" $
             classify (depthTT y > 3) "snd depth > 3" $
             checkRead readsPrec (x, y)

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
  \x -> checkRead readsPrec x

-- | property to test `T` read.
prop_readT :: Property
prop_readT = forAll (arbitrary :: Gen T) $
  \x -> classify (isTP x) "T P" $
        checkRead readsPrec x

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
         checkRead readsPrec xs

-- | quickcheck property to test `T` list read.
prop_readTList :: Property
prop_readTList = forAll (genList :: Gen [T]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         checkRead readsPrec xs

-- | quickcheck property to test `P` tuple read.
prop_readPTuple :: Property
prop_readPTuple = forAll (genTuple :: Gen (P, P)) $
  \(x, y) -> checkRead readsPrec (x, y)

-- | quickcheck property to test `T` tuple read.
prop_readTTuple :: Property
prop_readTTuple = forAll (genTuple :: Gen (T, T)) $
  \(x, y) -> classify (isTP x) "fst is T P" $
             classify (isTP y) "snd is T P" $
             checkRead readsPrec (x, y)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | QuickCheck tests for `Expr` read.
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `Expr`.
instance Arbitrary (Expr) where
  arbitrary = sized genExpr

-- | helper functions for implementing arbitrary.
-- | generator for `Expr` type.
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
genExpr :: Int -> Gen Expr
genExpr 0 = liftM Const (arbitrary :: Gen Int)
genExpr n = frequency
      [ (1, liftM Const (arbitrary :: Gen Int))
      -- NOTE: originally, i had divided by 2, but that generates very large 
      -- nested expressions, and quickcheck takes 8 - 10 minutes to run; 
      -- dividing by 3, on the other hand, runs the tests in < 20 secs, but also 
      -- generates reasonably sized nested expressions.
      , (2, liftM2 (:+:) (genExpr (n `div` 3)) (genExpr (n `div` 3)))
      , (2, liftM2 (:-:) (genExpr (n `div` 3)) (genExpr (n `div` 3)))
      , (2, liftM2 (:*:) (genExpr (n `div` 3)) (genExpr (n `div` 3)))
      , (2, liftM2 (:/:) (genExpr (n `div` 3)) (genExpr (n `div` 3)))
      ]

-- | valid `Expr` property.
prop_validExpr :: Property
prop_validExpr = forAll (arbitrary :: Gen Expr) $
  \x -> classify (isValid x) "valid Expr" $
        isValid x
  where isValid :: Expr -> Bool
        isValid (Const _) = True
        isValid (x :+: y) = isValid x && isValid y
        isValid (x :-: y) = isValid x && isValid y
        isValid (x :*: y) = isValid x && isValid y
        isValid (x :/: y) = isValid x && isValid y

-- | quickcheck property to test `Expr` read.
prop_readExpr :: Property
prop_readExpr = forAll (arbitrary :: Gen Expr) $
  \x -> classify (isConst x) "Const" $
        classify (depthExpr x > 3) "depth > 3" $
        classify (ldepthExpr x > 3) "left depth > 3" $
        classify (rdepthExpr x > 3) "right depth > 3" $
        checkRead readsPrec x

-- | some helper functions.
isConst :: Expr -> Bool
isConst (Const _) = True
isConst _         = False

depthExpr :: Expr -> Int
depthExpr (Const _) = 1
depthExpr (l :+: r) = 1 + max (depthExpr l) (depthExpr r)
depthExpr (l :-: r) = 1 + max (depthExpr l) (depthExpr r)
depthExpr (l :*: r) = 1 + max (depthExpr l) (depthExpr r)
depthExpr (l :/: r) = 1 + max (depthExpr l) (depthExpr r)

ldepthExpr :: Expr -> Int
ldepthExpr (Const _) = 1
ldepthExpr (l :+: _) = 1 + ldepthExpr l
ldepthExpr (l :-: _) = 1 + ldepthExpr l
ldepthExpr (l :*: _) = 1 + ldepthExpr l
ldepthExpr (l :/: _) = 1 + ldepthExpr l

rdepthExpr :: Expr -> Int
rdepthExpr (Const _) = 1
rdepthExpr (_ :+: r) = 1 + rdepthExpr r
rdepthExpr (_ :-: r) = 1 + rdepthExpr r
rdepthExpr (_ :*: r) = 1 + rdepthExpr r
rdepthExpr (_ :/: r) = 1 + rdepthExpr r

-- | property to test `Expr` read of random non-Expr string.
prop_readNonExprStr :: Property
prop_readNonExprStr = forAll (notExpr :: Gen String) $
  \x -> classify (x == "") "empty string" $
        classify (length x == 1) "1-elem string" $
        classify (length x > 1) "> 1 elem string" $
        (readsPrec 0 x :: [(Expr, String)]) == []
  where notExpr :: Gen String
        notExpr = frequency
            [ (1, (arbitrary :: Gen String)
                  `suchThat`
                  (\x -> not ("Const" `isInfixOf` x)))
            , (2, (arbitrary :: Gen String)
                  `suchThat`
                  (\x -> all (not . isDigit) x))
            ]

-- | quickcheck property to test `Expr` list read.
prop_readExprList :: Property
prop_readExprList = forAll (exprList :: Gen [Expr]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         classify (length xs > 3) "have > 3 elements" $
         checkRead readsPrec xs
  where exprList :: Gen [Expr]
        -- limit list size to <= 3, so that tests can be run quickly.
        exprList = (genList :: Gen [Expr])
                   `suchThat`
                   (\x -> (length x) <= 3)

-- | quickcheck property to test `Expr` tuple read.
prop_readExprTuple :: Property
prop_readExprTuple = forAll (genTuple :: Gen (Expr, Expr)) $
  \(x, y) -> classify (isConst x) "fst is Const _" $
             classify (isConst y) "snd is Const _" $
             checkRead readsPrec (x, y)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | QuickCheck tests for `Tree1` read.
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `Tree1`.
instance (Read a, Show a, Arbitrary a) => Arbitrary (Tree1 a) where
  arbitrary = sized genTree1

-- | helper functions for implementing arbitrary.
-- | generator for `Tree1` type.
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
genTree1 :: forall a. (Read a, Show a, Arbitrary a) => Int -> Gen (Tree1 a)
genTree1 0 = liftM Leaf1 (arbitrary :: Gen a)
genTree1 n = frequency
      [ (1, liftM Leaf1 (arbitrary :: Gen a))
      , (4, liftM2 Branch1 (genTree1 (n `div` 2)) (genTree1 (n `div` 2)))
      ]

-- | valid `Tree1` property.
prop_validTree1 :: Property
prop_validTree1 = forAll (arbitrary :: Gen (Tree1 String)) $
  \x -> classify (isValid x) "valid tree1" $
        isValid x
  where isValid :: Tree1 a -> Bool
        isValid (Leaf1 _)     = True
        isValid (Branch1 l r) = (isValid l) && (isValid r)

-- | quickcheck property to test `Tree1` read.
prop_readTree1 :: Property
prop_readTree1 = forAll (arbitrary :: Gen (Tree1 String)) $
  \x -> classify (isLeaf1 x) "Leaf1" $
        classify (depthT1 x > 3) "depth > 3" $
        classify (ldepthT1 x > 3) "left depth > 3" $
        classify (rdepthT1 x > 3) "right depth > 3" $
        checkRead readsPrec x

-- | some helper functions.
isLeaf1 :: Tree1 a -> Bool
isLeaf1 (Leaf1 _) = True
isLeaf1 _         = False

depthT1 :: Tree1 a -> Int
depthT1 (Leaf1 _)     = 1
depthT1 (Branch1 l r) = 1 + max (depthT1 l) (depthT1 r)

ldepthT1 :: Tree1 a -> Int
ldepthT1 (Leaf1 _)      = 1
ldepthT1 (Branch1 l _)  = 1 + ldepthT1 l

rdepthT1 :: Tree1 a -> Int
rdepthT1 (Leaf1 _)      = 1
rdepthT1 (Branch1 _ r)  = 1 + rdepthT1 r

-- | quickCheck property to test `Tree1` read for random non-Tree1 strings.
prop_readNonTree1Str :: Property
prop_readNonTree1Str = forAll (nonTree1 :: Gen String) $
  \x -> classify (x == "") "empty string" $
        classify (length x == 1) "1-elem str" $
        classify (length x > 1) "> 1 elem string" $
        (readsPrec 0 x :: [(Tree1 String, String)]) === []
  where nonTree1 :: Gen String
        nonTree1 = (arbitrary :: Gen String)
                   `suchThat`
                   (\x -> not ("<" `isInfixOf` x))

-- | quickcheck property to test `Tree1` list read.
prop_readTree1List :: Property
prop_readTree1List = forAll (genList :: Gen [Tree1 Int]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         checkRead readsPrec xs

-- | quickcheck property to test `Tree1` tuple read.
prop_readTree1Tuple :: Property
prop_readTree1Tuple = forAll (genTuple :: Gen (Tree1 String, Tree1 String)) $
  \(x, y) -> classify (isLeaf1 x) "fst is Leaf1" $
             classify (isLeaf1 y) "snd is Leaf1" $
             classify (depthT1 x > 3) "fst depth > 3" $
             classify (depthT1 y > 3) "snd depth > 3" $
             checkRead readsPrec (x, y)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | QuickCheck tests for `Time` read.
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `Time`.
instance Arbitrary (Time) where
  -- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
  arbitrary = frequency
      [ (1, liftM2 Time (chooseInt (0, 0)) (chooseInt (0, 0)))
      , (1, liftM2 Time (chooseInt (23, 23)) (chooseInt (59, 59)))
      , (4, liftM2 Time (chooseInt (0, 23)) (chooseInt (0, 59)))
      ]

-- | valid `Time` property.
prop_validTime :: Property
prop_validTime = forAll (arbitrary :: Gen Time) $
  \x -> classify (isValid x) "valid time" $
        isValid x
  where isValid :: Time -> Bool
        isValid (Time h m) = (h >= 0 && h <= 23) && (m >= 0 && m <= 59)

-- | quickcheck property to test `Time` read.
prop_readTime :: Property
prop_readTime = forAll (arbitrary :: Gen Time) $
  \(Time h m) -> classify (h == 0 && m == 0)  "hour = 0, minute = 0" $
                 classify (h == 23 && m == 59) "hour = 23, minute = 59" $
                 checkRead readsPrec (Time h m)

-- | quickCheck property to test `Time` read for random non-Time strings.
prop_readNonTimeStr :: Property
prop_readNonTimeStr = forAll (nonTime :: Gen String) $
  \x -> classify (x == "") "empty string" $
        classify (length x == 1) "1-elem str" $
        classify (length x > 1) "> 1 elem string" $
        (readsPrec 0 x :: [(Time, String)]) === []
  where nonTime :: Gen String
        nonTime = (arbitrary :: Gen String)
                  `suchThat`
                  (\x -> not (":" `isInfixOf` x))

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
