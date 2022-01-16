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
--
--  core ideas for these tests come from /u/ wes toleman on "how do you test a 
--  lexer using quickcheck?" @ https://tinyurl.com/2p8pt57w 
--  (softwareengineering.SE).

--------------------------------------------------------------------------------
module C2LexTest where
--------------------------------------------------------------------------------
-- Test.QuickCheck @ https://tinyurl.com/2p9s9ets
-- Data.List @ https://tinyurl.com/ycxb9uaw
-- Data.Char @ https://tinyurl.com/2c72x8ya
-- Text.Read @ https://tinyurl.com/36etaccn
-- Text.Read.Lex @ https://tinyurl.com/2tnj35ky
import Test.QuickCheck
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace, isAlpha, isDigit)
import Text.Read.Lex (Lexeme (Ident))

import C2Lex
import Common (ghciRunner)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | quickcheck testing -- lex stuff.
--------------------------------------------------------------------------------
-- | lex -- generators.
--------------------------------------------------------------------------------
-- | generate 1+ 'singles': haskell punctuation, underscore, apostrophe.
genSingles :: Gen String
genSingles = listOf1 $ (arbitrary :: Gen Char) `suchThat` isSingle
--------------------------------------------------------------------------------
-- | generate 1+ haskell 'symbols'.
genSyms :: Gen String
genSyms = listOf1 $ (arbitrary :: Gen Char) `suchThat` isSym
--------------------------------------------------------------------------------
-- | generate string that begins with 2 single quotes enclosing a character. the 
-- enclosed character is NOT a single quote.
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
-- | generate string that begins with a `"` & has 1+ `"` further on.
genDoubleQuotes :: Gen String
genDoubleQuotes = do
  str <- (arbitrary :: Gen String)
         `suchThat`
         (\xs -> ("\"" `isInfixOf` xs) && not ("\\" `isInfixOf` xs))
  return $ '\"' : str
--------------------------------------------------------------------------------
-- | generate string that begins with 1+ alphabets, followed by 0+ `id` chars.
genAlphas :: Gen String
genAlphas = do
  chs <- listOf1 $ (arbitrary :: Gen Char) `suchThat` isAlpha
  ch  <- elements chs
  ids <- listOf $ (arbitrary :: Gen Char) `suchThat` isIdChar
  return $ ch : ids
--------------------------------------------------------------------------------
-- | generate string that begins with 1+ digits and having no fractional part.
genDigits :: Gen String
genDigits = do
  chs <- listOf1 $ (arbitrary :: Gen Char) `suchThat` isDigit
  str <- (arbitrary :: Gen String)
         `suchThat`
         (all (not . (`elem` "eE.")))
  return $ chs ++ str
--------------------------------------------------------------------------------
-- | generate string that begins with 0+ space characters.
-- NOTE: 'space' here includes blanks as well as all other characters, such as 
-- '\n', '\t', that are regarded as spaces by `Data.Char.isSpace` function.
genSpaces :: Gen String
genSpaces = listOf $ (arbitrary :: Gen Char) `suchThat` isSpace
--------------------------------------------------------------------------------
-- | generate string that begins with a fractional in scientific notation.
--    1. all fractional numbers begin with 1 or more digits;
--    2. (1) may be followed by an optional decimal;
--    3. 1 or more digits follow (1-2);
--    4. next, we have the exponent, indicated by 'e' or 'E';
--    5. (4) may be followed by an optional '+' or '-';
--    6. (4-5) followed by 1 or more digits representing the exponent.
--    examples:
--      "123.45e+15"  "123.45E+15"
--      "12345e+15"   "12345E+15"
--      "12345e-15"   "12345E-15"
--      "12.345e15"   "12.345E15"
--      "12345e15"    "12345E15"
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
-- | generate 'bad' string that results in parse failure (i.e., []).
-- NOTE: to generate string that will result in parse failure, we eliminate all 
-- cases we know will result in good parse. this step by itself will go a long 
-- way, but it is not enough. so in addition to elimination of all good parses, 
-- we ensure generation of 2 known patterns that result in parse failures:
--    a) a string that starts with 2 single quotes enclosing a single quote;
--    b) a string that begins with a double quote but has none further on.
--
--    `l1` string generated in the code below ensures a); likewise, `l2` string 
--    ensures b). we use `frequency` to get a mix of both these patterns.
genBad :: Gen String
genBad = do
  l1 <- replicate 3 <$> ((arbitrary :: Gen Char) `suchThat` isBad)
  l2 <- listOf1 ((arbitrary :: Gen Char) `suchThat` isBad')
                `suchThat`
                (\xs -> case xs of
                      ('\'':x:'\'':_) -> x == '\''
                      ('\"':ys)       -> not ("\"" `isInfixOf` ys)
                      _               -> True)
  frequency [ (1, return l1)
            , (1, return l2)
            ]
  where isBad :: Char -> Bool
        isBad x = x /= '\"' && isBad' x
        isBad' :: Char -> Bool
        isBad' x = (not $ isSpace x) &&
                   (not $ isSingle x) &&
                   (not $ isSym x) &&
                   (not $ isAlpha x) &&
                   (not $ isDigit x)
--------------------------------------------------------------------------------
-- | append a random string of 0+ length to the supplied random string.
appendTo :: Gen String -> Gen String
appendTo g = do
  str1 :: String <- g
  str2 :: String <- listOf (arbitrary :: Gen Char)
  return $ str1 ++ str2
--------------------------------------------------------------------------------
-- | lex -- test the generators!
--------------------------------------------------------------------------------
-- | check if generated 'singles` string is valid.
prop_validSingles :: Property
prop_validSingles = forAll genSingles $
  \xs -> all isSingle xs
--------------------------------------------------------------------------------
-- | check if generated 'symbols' string is valid.
prop_validSyms :: Property
prop_validSyms = forAll genSyms $
  \xs -> all isSym xs
--------------------------------------------------------------------------------
-- | check if generated 'single-quotes' string is valid.
prop_validSingleQ :: Property
prop_validSingleQ = forAll genSingleQuotes $
  \xs -> case xs of
      ('\'':x:'\'':_)  -> x =/= '\'' .&&. x =/= '\\'
      _                -> property False
--------------------------------------------------------------------------------
-- | check if generated 'double-quotes' string is valid.
prop_validDoubleQ :: Property
prop_validDoubleQ = forAll genDoubleQuotes $
  \xs -> case xs of
      ('\"':ys)   -> "\"" `isInfixOf` ys && not ("\\" `isInfixOf` ys)
      _           -> False
--------------------------------------------------------------------------------
-- | check if generated 'alphas' string is valid.
prop_validAlphas :: Property
prop_validAlphas = forAll genAlphas $
  \x -> (isAlpha $ head x) && (all isIdChar $ tail x)
--------------------------------------------------------------------------------
-- | check if generated 'digits' string is valid.
prop_validDigits :: Property
prop_validDigits = forAll genDigits $
  \xs -> let (a, b) = span isDigit xs
         in a =/= [] .&&.
            (case b of
                ""    -> property True
                _     -> notFractional b)
  where notFractional :: String -> Property
        notFractional s = property $ all (not . (`elem` "eE.")) s
--------------------------------------------------------------------------------
-- | check if generated 'spaces' string is valid.
prop_validSpaces :: Property
prop_validSpaces = forAll genSpaces $
  \xs -> all isSpace xs
--------------------------------------------------------------------------------
-- | check if generated 'fractional' string is valid.
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
-- | check if generated 'bad' string is valid.
prop_validBad :: Property
prop_validBad = forAll genBad $
  \xs -> let (a1, a2) = span (== '\"') xs
             (b1, b2) = span isSpace xs
             (c1, c2) = span isSingle xs
             (d1, d2) = span isSym xs
             (e1, e2) = span isAlpha xs
             (f1, f2) = span isDigit xs
         in if length xs == 3   -- 3-element string generated should have no ".
              then (a1 ++ b1 ++ c1 ++ d1 ++ e1 ++ f1) === [] .&&.
                   (all (== xs) [a2, b2, c2, d2, e2, f2])
              else (b1 ++ c1 ++ d1 ++ e1 ++ f1) === [] .&&.
                   (all (== xs) [b2, c2, d2, e2, f2]) .&&.
                   case (a1, a2) of
                      ("\"", _)  -> property $ not $ "\"" `isInfixOf` a2
                      ("", _)    -> a2 === xs
                      _          -> property False

--------------------------------------------------------------------------------
-- | lex -- properties
--------------------------------------------------------------------------------
-- NOTE: `Lexeme` defined in `Text.Read.Lex`.
--  data Lexeme
--      = Char Char
--      | String String
--      | Punc String
--      | Ident String
--      | Symbol String
--      | Number Text.Read.Lex.Number
--      | EOF
--      deriving (Eq, Show, Read)
--------------------------------------------------------------------------------
-- | check `lex'` parse of string starting with 1+ 'singles'.
prop_single :: Property
prop_single = forAll (appendTo genSingles) $
  \xs -> case (lex' xs) of
          ((a1, a2) : []) -> (a1 ++ a2) === xs .&&.
                              length a1 === 1 .&&.
                              (property . isSingle . head $ a1)
          _               -> property False
--------------------------------------------------------------------------------
-- | check `lex'` parse of string that starts with 1+ 'symbols'.
-- REF: about `\&`, see /u/ chi @ https://tinyurl.com/bder5brs (so)
prop_sym :: Property
prop_sym = forAll (appendTo genSyms) $
  \xs -> classify ("\\&" `isPrefixOf` xs) "begins with \\&" $
         classify ("\\&" `isInfixOf` xs) "has \\&" $
         classify ("\\" `isInfixOf` xs) "has \\" $
         classify ("\\" `isPrefixOf` xs) "begins with \\" $
         case (lex' xs) of
          ((a1, a2) : []) -> (a1 ++ a2) === xs .&&.
                             (case a1 of
                                []    -> property False
                                _     -> property $ all isSym a1) .&&.
                             (case a2 of
                                ""      ->  property True
                                (y:_)   ->  property $ not $ isSym y)
          _               -> property False
--------------------------------------------------------------------------------
-- | check `lex'` parse of string that starts with a pair of single quotes 
-- enclosing a non-single quote character.
--
-- we use both a post-condition and a model to test this property.  the model 
-- comes from `read` & `readsPrec` functions in `Read` instance for `Char`.
--
-- NOTE: we use `read l1 :: Char` for the following reason.  for example:
--      `lex' "'p'rem" = [("'p'", "rem")]`
--      `readsPrec 0 "'p'rem" :: [(Char, String)]
--          = [('p', "rem")]`
--       so to compare `lex'` results to `readsPrec` one, we have to convert 
--       "'p'" to 'p' somewhow.  we use `read` to do that.
--       `read "'p'" :: Char = 'p'`
prop_singleQuotes :: Property
prop_singleQuotes = forAll genSingleQuotes $
  \xs -> case (lex' xs) of
          ((a1, a2) : []) -> (a1 ++ a2) === xs .&&. asRead xs (a1, a2)
          _               -> property False
  where asRead :: String -> (String, String) -> Property
        asRead s (l1, l2) = let rs  = readsPrec 0 s :: [(Char, String)]
                                r1' = read l1 :: Char
                            in case rs of
                                ((r1, r2) : []) -> r1 === r1' .&&. r2 === l2
                                _               -> property False
--------------------------------------------------------------------------------
-- | check `lex'` parse of string that starts with a double quote and has 1+ 
-- double quotes further on.
--
-- we use both a post-condition and a model to test this property.  the model 
-- comes from `read` & `readsPrec` functions in `Read` instance for `String`.
--
-- NOTE: we use `read l1 :: String` for the following reason.  for example:
--      `lex' "\"prem\"muthedath" = [("\"prem\"", "muthedath")]`
--      `readsPrec 0 "\"prem\"muthedath" :: [(String, String)]
--          = [("prem", "muthedath")]`
--       so to compare `lex'` results to `readsPrec` one, we have to convert 
--       "\"prem\"" to "prem" somewhow.  we use `read` to do that.
--       `read "\"prem\"" :: String = "prem"`
prop_doubleQuotes :: Property
prop_doubleQuotes = forAll (appendTo genDoubleQuotes) $
  \xs -> case (lex' xs) of
          ((a1, a2) : []) -> (a1 ++ a2) === xs .&&. asRead xs (a1, a2)
          _               -> property False
  where asRead :: String -> (String, String) -> Property
        asRead s (l1, l2) = let rs  = readsPrec 0 s :: [(String, String)]
                                r1' = read l1 :: String
                            in case rs of
                                ((r1, r2) : []) -> r1 === r1' .&&. r2 === l2
                                _               -> property False
--------------------------------------------------------------------------------
-- | check `lex'` parse of string starting with 1 or more alphabets, followed by 
-- 0 or more `id` characters.
--
-- we use both a post-condition and a model to test this property.  the model 
-- comes from `read` & `readsPrec` functions in `Read` instance for `Lexeme`.  
-- since alphabets in haskell typically form identifiers, we use the `Ident`
-- constructor defined for `Lexeme`.
prop_alpha :: Property
prop_alpha = forAll (appendTo genAlphas) $
  \xs -> case (lex' xs) of
          ((a1, a2) : []) -> (a1 ++ a2) === xs .&&. asRead xs (a1, a2)
          _               -> property False
  where asRead :: String -> (String, String) -> Property
        asRead s (l1, l2) = let rs = readsPrec 0 s :: [(Lexeme, String)]
                            in case rs of
                                ((Ident r1, r2) : []) -> r1 === l1 .&&. r2 === l2
                                _                     -> property False
--------------------------------------------------------------------------------
-- | check `lex'` parse of string starting with 1+ digits, with no fractional.
-- we use both a post-condition and a model to test this property.  the model 
-- comes from `read` & `readsPrec` functions in `Read` instance for `Integer`.
prop_digit :: Property
prop_digit = forAll genDigits $
  \xs -> case (lex' xs) of
          ((a1, a2) : []) -> (a1 ++ a2) === xs .&&.asRead xs (a1, a2)
          _               -> property False
  where asRead :: String -> (String, String) -> Property
        asRead s (l1, l2) = let rs  = readsPrec 0 s :: [(Integer, String)]
                                r1' = read l1 :: Integer
                            in case rs of
                                ((r1, r2) : []) -> r1 === r1' .&&. r2 === l2
                                _               -> property False
--------------------------------------------------------------------------------
-- | check `lex'` parse of string that starts with 0 or more space characters.
-- NOTE: space characters include blanks as well as '\n', '\r', '\t', etc; that 
-- is, anything that returns `True` from `Data.Char.isSpace`.
prop_space :: Property
prop_space = forAll (appendTo genSpaces) $
  \xs -> classify (xs == "") "empty string" $
         lex' xs === (lex' $ dropWhile isSpace xs)
--------------------------------------------------------------------------------
-- | check `lex'` parse of string representing a fractional number.
-- we use both a post-condition and a model to test this property.  the model 
-- comes from `read` & `readsPrec` functions in `Read` instance for `Double`.
--
-- NOTE: for large numbers, `read` returns `Infinity`, but even then this test 
-- is pretty good, because it is consistent: if `lex' xs = [(a1, a2)]`, we still 
-- have `read a1 == r1`, & `a2  == r2`, where `[(r1, r2)] = readsPrec 0 xs`.
prop_fractional :: Property
prop_fractional = forAll (appendTo genFractional) $
  \xs -> case (lex' xs) of
            ((a1, a2) : []) -> (a1 ++ a2) === xs .&&. asRead xs (a1, a2)
            _               -> property False
  where asRead :: String -> (String, String) -> Property
        asRead s (l1, l2) = let rs  = readsPrec 0 s :: [(Double, String)]
                                r1' = read l1 :: Double
                            in case rs of
                                ((r1, r2) : []) -> r1 === r1' .&&. r2 === l2
                                _               -> property False
--------------------------------------------------------------------------------
-- | check `lex'` parse of string starting with invalid token.
prop_bad :: Property
prop_bad = forAll genBad $
  \xs -> classify ("'''" `isPrefixOf` xs) "begins with '''" $
         classify ("\"" `isPrefixOf` xs) "begins with double quote" $
         lex' xs === []
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
