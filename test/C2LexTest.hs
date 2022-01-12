{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | quickcheck tests for ../src/C2Lex.hs
-- author: Prem Muthedath, JAN 2022.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler-test` to start GHCi.
--  3. at GHCi prompt, enter `import C2LexTest`.
--  4. you can then invoke `C2LexTest.ghciQC` to run all quickcheck tests.
--
--  REF: "A Guide to Writing Properties of Pure Functions", John Hughes.
--  https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf

--------------------------------------------------------------------------------
module C2LexTest where
--------------------------------------------------------------------------------
-- Test.QuickCheck @ https://tinyurl.com/2p9s9ets
-- Data.List @ https://tinyurl.com/ycxb9uaw
-- Data.Char @ https://tinyurl.com/2c72x8ya
import Test.QuickCheck
import Data.List (isInfixOf)
import Data.Char (isSpace, isAlpha, isDigit)

import C2Lex
import Common (ghciRunner)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | quickcheck testing -- lex stuff.
--------------------------------------------------------------------------------
-- | lex -- generators.
--------------------------------------------------------------------------------
genSingle :: Gen String
genSingle = listOf1 $ (arbitrary :: Gen Char) `suchThat` isSingle
--------------------------------------------------------------------------------
genSym :: Gen String
genSym = listOf1 $ (arbitrary :: Gen Char) `suchThat` isSym
--------------------------------------------------------------------------------
genSingleQuotes :: Gen String
-- NOTE: previously, we had used `suchThat` on the generated list, but it was a 
-- bit slow.  see the commented code below.
--    listOf (arbitrary :: Gen Char) `suchThat`
--      \ys -> case ys of
--        ('\'':y:'\'':_) -> (y /= '\'') && (y /= '\\')
--        _               -> False
-- now, we use `suchThat` on the generated `Char`, and then build the 
-- list with it.  of course, this algorithm is super fast.
genSingleQuotes = do
  ch  <- (arbitrary :: Gen Char)
         `suchThat`
         (\x -> x /= '\'' && x /= '\\')
  chs <- listOf (arbitrary :: Gen Char)
  return $ ['\'', ch, '\''] ++ chs
--------------------------------------------------------------------------------
genDoubleQuotes :: Gen String
genDoubleQuotes = do
  str <- (arbitrary :: Gen String)
         `suchThat`
         (\xs -> ("\"" `isInfixOf` xs) && not ("\\" `isInfixOf` xs))
  return $ '\"' : str
--------------------------------------------------------------------------------
genAlpha :: Gen String
genAlpha = do
  chs <- listOf1 $ (arbitrary :: Gen Char) `suchThat` isAlpha
  ch  <- elements chs
  ids <- listOf $ (arbitrary :: Gen Char) `suchThat` isIdChar
  return $ ch : ids
--------------------------------------------------------------------------------
genDigit :: Gen String
genDigit = do
  chs <- listOf1 $ (arbitrary :: Gen Char) `suchThat` isDigit
  str <- (arbitrary :: Gen String)
         `suchThat`
         (\xs -> all (\x -> not $ x `elem` ['e', 'E', '.']) xs)
  return $ chs ++ str
--------------------------------------------------------------------------------
genSpaces :: Gen String
genSpaces = listOf $ (arbitrary :: Gen Char) `suchThat` isSpace
--------------------------------------------------------------------------------
genFractional :: Gen String
genFractional = do
  a <- listOf1 $ (arbitrary :: Gen Char) `suchThat` isDigit
  b <- listOf (elements ['.'])
       `suchThat`
       (\xs -> length xs <= 1)
  c <- listOf1 $ (arbitrary :: Gen Char) `suchThat` isDigit
  d <- elements ['e', 'E']
  e <- listOf (elements ['+', '-'])
       `suchThat`
       (\xs -> length xs <= 1)
  f <- listOf1 $ (arbitrary :: Gen Char) `suchThat` isDigit
  return $ a ++ b ++ c ++ [d] ++ e ++ f
--------------------------------------------------------------------------------
genBad :: Gen String
genBad = listOf1 ((arbitrary :: Gen Char)
                  `suchThat`
                  (\x -> x /= '\'' &&
                         x /= '\"' &&
                         (not $ isSpace x) &&
                         (not $ isSingle x) &&
                         (not $ isSym x) &&
                         (not $ isAlpha x) &&
                         (not $ isDigit x)))
--------------------------------------------------------------------------------
appendTo :: Gen String -> Gen String
appendTo g = do
  str1 :: String <- g
  str2 :: String <- listOf (arbitrary :: Gen Char)
  return $ str1 ++ str2
--------------------------------------------------------------------------------
-- | lex -- test the generators!
--------------------------------------------------------------------------------
prop_validSingle :: Property
prop_validSingle = forAll genSingle $
  \xs -> all isSingle xs
--------------------------------------------------------------------------------
prop_validSym :: Property
prop_validSym = forAll genSym $
  \xs -> all isSym xs
--------------------------------------------------------------------------------
prop_validSingleQ :: Property
prop_validSingleQ = forAll genSingleQuotes $
  \xs -> case xs of
      ('\'':x:'\'':_)  -> x /= '\'' && x /= '\\'
      _                -> False
--------------------------------------------------------------------------------
prop_validDoubleQ :: Property
prop_validDoubleQ = forAll genDoubleQuotes $
  \xs -> case xs of
      ('\"':ys)   -> "\"" `isInfixOf` ys && not ("\\" `isInfixOf` ys)
      _           -> False
--------------------------------------------------------------------------------
prop_validAlpha :: Property
prop_validAlpha = forAll genAlpha $
  \x -> (isAlpha $ head x) && (all isIdChar $ tail x)
--------------------------------------------------------------------------------
prop_validDigit :: Property
prop_validDigit = forAll genDigit $
  \xs -> let (a, b) = span isDigit xs
         in a =/= [] .&&.
            (case b of
                ""    -> property True
                _     -> notFractional b) .&&.
            xs === (a ++ b)
  where notFractional :: String -> Property
        notFractional s = property $ all (\x -> not $ x `elem` "eE.") s
--------------------------------------------------------------------------------
prop_validSpace :: Property
prop_validSpace = forAll genSpaces $
  \xs -> all isSpace xs
--------------------------------------------------------------------------------
prop_validFractional :: Property
prop_validFractional = forAll genFractional $
  \xs -> let (a, b) = span isDigit xs
             (c, d) = span (== '.') b
         in a =/= [] .&&.
            b =/= [] .&&.
            (case c of
              []        -> isExp b
              ('.':[])  -> isFracExp d
              _         -> property False)
  where isExp :: String -> Property
        isExp xs = case xs of
            ('e':'+':s) -> f s
            ('e':'-':s) -> f s
            ('e':s)     -> f s
            ('E':'+':s) -> f s
            ('E':'-':s) -> f s
            ('E':s)     -> f s
            _           -> property False
            where f :: String -> Property
                  f s' = s' =/= [] .&&. property (all isDigit s')
        isFracExp :: String -> Property
        isFracExp xs =
          let (a1, a2) = span isDigit xs
          in a1 =/= [] .&&. isExp a2
--------------------------------------------------------------------------------
prop_validBad :: Property
prop_validBad = forAll genBad $
  \xs -> let (a1, a2) = span (== '\'') xs
             (b1, b2) = span (== '\"') xs
             (c1, c2) = span (== ' ') xs
             (d1, d2) = span isSingle xs
             (e1, e2) = span isSym xs
             (f1, f2) = span isAlpha xs
             (g1, g2) = span isDigit xs
         in (a1 ++ b1 ++ c1 ++ d1 ++ e1 ++ f1 ++ g1) === [] .&&.
            (all (== xs) [a2, b2, c2, d2, e2, f2, g2])
--------------------------------------------------------------------------------
-- | lex -- properties
--------------------------------------------------------------------------------
prop_single :: Property
prop_single = forAll (appendTo genSingle) $
  \x -> case (lex' x) of
          ((a1, a2) : []) -> (a1 ++ a2) === x .&&.
                              length a1 === 1 .&&.
                              (property $ all isSingle a1)
          _               -> property False
--------------------------------------------------------------------------------
prop_sym :: Property
prop_sym = forAll (appendTo genSym) $
  \x -> case (lex' x) of
          ((a1, a2) : []) -> (a1 ++ a2) === x .&&.
                             (case a1 of
                                []    -> property False
                                _     -> property $ all isSym a1) .&&.
                             (case a2 of
                                ""      ->  property True
                                (y:_)   ->  property $ not $ isSym y)
          _               -> property False
--------------------------------------------------------------------------------
prop_singleQuote :: Property
prop_singleQuote = forAll genSingleQuotes $
  \x -> case (lex' x) of
          ((a1, a2) : []) -> (a1 ++ a2) === x .&&.
                             (case a1 of
                                ('\'':_:'\'':[]) -> property True
                                _                -> property False)
          _               -> property False
--------------------------------------------------------------------------------
prop_doubleQuote :: Property
prop_doubleQuote = forAll genDoubleQuotes $
  \x -> case (lex' x) of
          ((a1, a2) : []) -> (a1 ++ a2) === x .&&.
                             (case a1 of
                                (_:[])    -> property False
                                ('\"':ys) -> endDoubleQ ys
                                _         -> property False)
          _               -> property False
  where endDoubleQ :: String -> Property
        endDoubleQ s = (last s === '\"') .&&.
                       property (not $ any (== '\"') $ init s)
--------------------------------------------------------------------------------
prop_alpha :: Property
prop_alpha = forAll (appendTo genAlpha) $
  \x -> case (lex' x) of
          ((a1, a2) : []) -> (a1 ++ a2) === x .&&.
                             (case a1 of
                                (y:ys)    -> property $
                                              isAlpha y && all isIdChar ys
                                _         -> property False)
          _               -> property False
--------------------------------------------------------------------------------
prop_digit :: Property
prop_digit = forAll genDigit $
  \x -> case (lex' x) of
          ((a1, a2) : []) -> (a1 ++ a2) === x .&&.
                             (case a1 of
                                []        -> property False
                                _         -> property $ all isDigit a1) .&&.
                             (case a2 of
                                []        -> property True
                                (y:_)     -> property $ not $ isDigit y)
          _               -> property False
--------------------------------------------------------------------------------
prop_space :: Property
prop_space = forAll (appendTo genSpaces) $
  \x -> lex' x === (lex' $ dropWhile isSpace x)
--------------------------------------------------------------------------------
prop_fractional :: Property
prop_fractional = forAll (appendTo genFractional) $
  \x -> case (lex' x) of
          ((a1, a2) : []) -> (a1 ++ a2) === x .&&.
                             (case a1 of
                                []        -> property False
                                _         -> property $
                                              (isDigit $ head a1) .&&.
                                              all (`elem` allowables) a1) .&&.
                             (case a2 of
                                []        -> property True
                                (y:_)     -> property . not . isDigit $ y)
          _               -> property False
  where allowables :: String
        allowables = "0123456789eE+-."
--------------------------------------------------------------------------------
prop_bad :: Property
prop_bad = forAll genBad $
  \xs -> lex' xs === []
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
