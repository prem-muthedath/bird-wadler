-- | chapter 2: bird & wadler, introduction to functional programming.
-- `lex`, Haskell Report, 2010, chapter 9. https://tinyurl.com/2p8wdy3c
-- see also Text.Read @ https://tinyurl.com/36etaccn
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler` to start GHCi.
--  3. next, at GHCi prompt, enter `import C2Lex`.
--  4. you can then invoke any of the top-elevel functions.
--  5. OR instead of steps 2-4,
--      a) run `cabal v2-repl :bird-wadler-test` to start GHCi;
--      b) at GHCi prompt, enter `import C2LexTest`;
--      c) invoke `C2LexTest.ghciQC` to run all quickcheck tests.
-- author: Prem Muthedath, NOV 2021.

--------------------------------------------------------------------------------
module C2Lex where
--------------------------------------------------------------------------------
-- Data.Char @ https://tinyurl.com/2c72x8ya
-- Numeric @ https://tinyurl.com/yckpd53t
import Data.Char (lexLitChar, isSpace, isDigit, isAlpha, isAlphaNum)
import Numeric (lexDigits)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- |`lex` reads a single lexeme from the input, discarding initial white space, 
-- returning the characters that constitute the lexeme. if the input string 
-- contains only white space, `lex` returns a single successful 'lexeme' 
-- consisting of the empty string -- `lex "" = [("", "")]`. if there is no legal 
-- lexeme at the beginning of the input string, `lex` fails (i.e., returns []).
-- code from Haskell 2010 Report, https://tinyurl.com/2p8wdy3c
lex'            :: ReadS String
lex' ""         =  [("","")]
lex' (c:s)
  | isSpace c   =  lex' (dropWhile isSpace s)
lex' ('\'':s)   =  [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
                                        ch /= "'" ]
lex' ('"':s)    =  [('"':str, t) | (str,t) <- lexString s]
lex' (c:s)
  | isSingle c  = [([c],s)]
  -- span :: (a -> Bool) -> [a] -> ([a], [a])
  -- span (\x -> x > 2) [1 :: Int, 2, 3, 4] = ([],[1,2,3,4])
  -- span (\x -> x <= 2) [1 :: Int, 2, 3, 4] = ([1, 2],[3,4])
  | isSym c     = [(c:sym,t) | (sym,t) <- [span isSym s]]
  | isAlpha c   = [(c:nam,t) | (nam,t) <- [span isIdChar s]]
  | isDigit c   = [(c:ds++fe,t) | (ds,s1) <- [span isDigit s],
                                  (fe,t)  <- lexFracExp s1]
  | otherwise   = []    -- bad character

--------------------------------------------------------------------------------
-- | parse string that contains a `"`.
-- if supplied string has an `"`, then the substring from start to `"` is 
-- returned; for example, `lexString "prem\"toss" = [("prem\"", "toss")]`.
-- returns [], a parse failure, for any string supplied to it that does not have 
-- an `"`. this happens because as `lexString` parses the string, it finally 
-- reaches the end of the string, `""`, but `lexLitChar "" = []`, a parse 
-- failure, which results in `lexStrItem "" = []`, again a parse failure, 
-- resulting in `lexString` returning [].
lexString               :: ReadS String
lexString ('"':s)       = [("\"",s)]      -- recurrence terminal condition
lexString s             = [(ch++str, u) | (ch,t)  <- lexStrItem s,
                                          (str,u) <- lexString t ]
-- | parse a string item.
-- see `lexLitChar` in Data.Char @ https://tinyurl.com/2c72x8ya
-- `lexLitChar` reads a character as a string, using haskell escape conventions.
-- `lexLitChar :: ReadS String`
-- `lexLitChar  "\\nHello"  =  [("\\n", "Hello")]`
-- `lexLitChar "prem" = ("p", "rem")`
-- `lexLitChar` removes any nullables "\\&" present.
-- `lexLitChar "" = []` => a parse failure
-- `lexStrItem "" = []` => a parse failure
-- REF: about `\&`, see /u/ chi @ https://tinyurl.com/bder5brs (so)
lexStrItem              :: ReadS String
lexStrItem ('\\':'&':s) = [("\\&",s)]
lexStrItem ('\\':c:s) | isSpace c   -- "\\   \\prem" => ("\\&", "prem")
                        = [("\\&",t) | '\\':t <- [dropWhile isSpace s]]
lexStrItem a            = lexLitChar a

--------------------------------------------------------------------------------
-- | is the character a member of "special" characters in haskell?
-- see https://tinyurl.com/yc8y5tfs
isSingle    :: Char -> Bool
isSingle c  =  c `elem` ",;()[]{}_‘"

-- | is the character a haskell-recognized symbol?
isSym   :: Char -> Bool
isSym c =  c `elem` "!@#$%&⋆+./<=>?\\^|:-~"

-- | is the character in the set of allowed id characters in haskell?
isIdChar    :: Char -> Bool
isIdChar c  =  isAlphaNum c || c `elem` "_'"

--------------------------------------------------------------------------------
-- | `lex` a string that represents a fraction.
-- first part checks decimal point and after; if found, it extracts decimal & 
-- all digits following decimal & then parses exponent, if any. if no decimal, 
-- it goes to the 2nd part, which simply parses the exponent, indicated by "eE".

-- valid inputs should begin with either a decimal, which may contain an 
-- exponent "eE", OR an exponent "eE". other inputs will simply return [("", 
-- str)], where `str` is the original string.
-- sample inputs, outputs:
--    1. ".123e+10"  ->  [(".123e+10","")]
--    2. ".123E10"   ->  [(".123E10","")]
--    3. ".123+10"   ->  [(".123","+10")]  -- BAD EXPONENT!
--    4. "123e+10"   ->  [("","123e+10")]  -- BAD: NO DECIMAL OR EXP @ START!
--    5. ".123"      ->  [(".123","")]
--    6. "E-10"      ->  [("E-10","")]
--    7. "123"       ->  [("","123")]      -- BAD: NOT A FRACTION!
--    8. "abc"       ->  [("", "abc")]     -- BAD: NOT A FRACTION!
--    9. ""          ->  [("", "")]
--    10. "12.3e+10" ->  [("","12.3e+10")] -- BAD: NO DECIMAL OR EXP @ START!
-- `lexDigits` is defined in `Numeric` module: https://tinyurl.com/yckpd53t
-- `lexDigits :: ReadS String`
-- `lexDigits "123pr" = [("123","pr")]`
-- `lexDigits "pr656" = []`
lexFracExp    :: ReadS String
lexFracExp ('.':c:cs) | isDigit c
              = [('.':ds++e,u) | (ds,t) <- lexDigits (c:cs),
                                 (e,u)  <- lexExp t]
lexFracExp s  = lexExp s

-- | `lex` the exponent string of a fraction.
-- valid inputs must begin with the exponent -- i.e., "eE", which may be 
-- immediately followed by "+-"; if not, it returns [("", str)], where `str` is 
-- the original string.
-- sample inputs, outputs:
--    1.  "e+10"      ->  [("e+10","")]
--    2.  "E10"       ->  [("E10","")]
--    3.  "123"       ->  [("","123")]      -- BAD EXPONENT!
--    4.  "+123"      ->  [("","+123")]     -- BAD EXPONENT!
-- note that the 2 lists joined by `++` are mutually exclusive; the first one 
-- succeeds for "+-", while the 2nd one succeeds when "+-" is not there.
-- `lexDigits` is defined in `Numeric` module: https://tinyurl.com/yckpd53t
lexExp      :: ReadS String
lexExp (e:s) | e `elem` "eE"
            = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
                            (ds,u) <- lexDigits t] ++
              [(e:ds,t)   | (ds,t) <- lexDigits s]
lexExp s    = [("",s)]

--------------------------------------------------------------------------------
