-- | chapter 2: bird & wadler, introduction to functional programming.
-- string data type: example.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler` to start GHCi.
--  3. next, at GHCi prompt, enter `import C2String`.
--  4. you can then invoke any of the top-elevel functions.
-- Prem Muthedath, 20 OCT 2021.

--------------------------------------------------------------------------------
-- | notes (from bird & wadler)
-- strings are sequences of characters; they are denoted using double quotation 
-- marks. difference between 'a' & "a" is that the former is a character, while 
-- the latter is a list of characters that happen to contain only 1 element.
--
-- comparisions on strings follow lexicographic ordering:
-- "hello" < "hallo" => False
-- "Jo" < "Joanna"   => True
--
-- the most important feature of strings is how they are printed:
-- "a"      => a
-- "hello"  => hello
-- "this sentence contains a \n." => this sentence contains a
-- .
--unlike any other data type, strings are printed literally. this means: (i) 
--double quotation marks do not appear in the output; and (ii) special
--characters, such as '\n', are printed as the actual character they represent.  
--this convention for strings gives complete control over layout of results.
--
-- layout:
-- with function `show`, the printing convention for strings described above 
-- gives us all the control we need to produces tables, formatted text, etc.
--
-- the function `show` takes an arbitrary values as argument, (i.e., instances 
-- of `Show`), and convert it into a printable representation.
--
--        show :: Show a => a -> String
--
--        show (42 :: Int) = "42"
--        show True        = "True"
--        show 'a'         = "'a'"
--        show "hello"     = "\"hello\""
--
-- if the result of an evaluation is not a string, then the evaluator 
-- automatically applies the function `show`.  if it is a string, then it is 
-- printed literally. so we have:
--
-- "me how"           => me how
-- show "me how"      => "\"me how\""
-- show ("me". "how") => "(\"me\",\"how\")"
--
-- "this year is " ++ show (3 * 667 :: Int) = "this year is 2001"
-- show $ "this year is " ++ show (3 * 667 :: Int) = "\"this year is 2001\""
--
-- NOTE: haskell's `show` function converts strings to their unicode 
-- representation. see /u/ chris taylor @ https://tinyurl.com/mrj33au2 (so)
--
-- NOTE: what is the difference between haskell's `putStrLn` & `print`
-- functions? see /u/ chris taylor @ https://tinyurl.com/mrj33au2 (so)
--
-- when printing strings, it is useful to have control over just where on the 
-- line the value of the expression appears. more often, we want the value to 
-- appear on the left (left-justified), or on the right (right-justified), or in 
-- the center (center-justified). we define functions:
--
--    ljustify, cjustify, rjustify :: Int -> String -> String
--
--  so that (ljustify n x) is the string x padded with extra spaces on the right 
--  to make a string of total width n, (cjustify n x) is the string x centered 
--  with spaces on both sides, and (rjustify n x) is x with spaces on the left.
--
--  to define these functions, we suppose existence of a function `width` which 
--  returns the `width` of a string when printed. this is a measure of the 
--  horizontal space occupied by the string. for a string of characters in a 
--  fixed-width font, `width` just returns the number of characters in the 
--  string. we also define a `space` function so that `space n` returns a string 
--  of space characters whose width is `n`. again, for fixed-width font, this 
--  will be just a string containing `n` spaces.
--
--  ljustify n x = x ++ space (n - m), if n >= m
--                 where m = width x
--  rjustify n x = space (n - m) ++ x, if n >= m
--                 where m = width x
--  cjustify n x = space lm ++ x ++ space rm, if n >= m
--                 where m  = width x
--                       lm = (n - m) `div` 2
--                       rm = (n - m) - lm
--
-- all 3 functions are partial, returning _|_ if the string is too long to fit 
-- in the given width.
--------------------------------------------------------------------------------
-- | ByteString
-- REF: ByteString @ hackage:
--        https://hackage.haskell.org/package/bytestring
--      Word @ hackage:
--        https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Word.html
--      Bits @ hackage:
--        https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Bits.html
--      Data.Text vs Data.ByteString.Char8:
--        see /u/ ehird @ https://tinyurl.com/2p984xzs (so)
--      Char to Word8 ++ Text vs ByteString:
--        /u/ ehird @ https://tinyurl.com/5k2534hm (so)
--      ByteString tutorial by luke taylor:
--        https://www.schoolofhaskell.com/user/tekul/bytestring-bits-and-pieces
--      binary-to-text encoding:
--        https://en.wikipedia.org/wiki/Binary-to-text_encoding
--      bit numbering:
--        https://en.wikipedia.org/wiki/Bit_numbering
-- an efficient compact, immutable byte string type (both strict & lazy) 
-- suitable for binary or 8-bit character data.
--  a) `ByteString` type represents sequence of bystes or 8-bit characters;
--  b) suitable for high performance use: large quantities, or high speeds;
--  c) functions use list style, so easy to convert `String` to `ByteString`;
--  d) `Strict` `ByteString` keeps the string as a single large array;
--  e) `Lazy` `ByteString` uses a lazy list of strict chunks, suited for large 
--     data requirements, including IO/streaming tasks;
--  f) `Char8` module provides a character-based view of the same underlying 
--     `ByteString` types -- so you can handle mixed binary & 8-bit character 
--     contents common in many file formats & network protocols;
--  g) `Builder` module: efficient way to build up `ByteString` in an ad-hoc way 
--     by repeated concatenation, ideal for serilization & pretty printing;
--  h) `ByteString` not designed for Unicode; use `Text` instead.
--  i) `ByteString` modules require `qualified` imports.
--
--  characters or bytes?
--  depending on the context, we may prefer to view the `ByteString` as made up 
--  of a list of 8-bit `Char` or of `Word8`, haskell's standard representation 
--  of a byte (an unsigned integer type). there is only one `ByteString` data 
--  structure for both, but the library exposes different functions depending on 
--  how we want to interpret the contents. Data.ByteString` provides the `Word8` 
--  functions while `Data.ByteString.Char8` provides the `Char` equivalents, 
--  which by the way work only with `ASCII` text.
--------------------------------------------------------------------------------
module C2String where
--------------------------------------------------------------------------------
-- | bird & wadler
--------------------------------------------------------------------------------
-- | width of a given string.
width :: String -> Int
width [] = 0
width (_:xs) = 1 + width xs
--------------------------------------------------------------------------------
-- | returns a string of space characters whose width is `n`.
space :: Int -> String
space 0 = ""
space n = " " ++ space (n-1)
--------------------------------------------------------------------------------
-- | left-justifies a string.
ljustify :: Int -> String -> String
ljustify n x | n >= m    = x ++ space (n - m)
             | otherwise = error "ljustify failed because string is too long."
             where m :: Int
                   m = width x
--------------------------------------------------------------------------------
-- | right-justifies a string.
rjustify :: Int -> String -> String
rjustify n x | n >= m     = space (n - m) ++ x
             | otherwise  = error "rjustify failed because string is too long."
             where m :: Int
                   m = width x
--------------------------------------------------------------------------------
-- | center-justifies a string.
cjustify :: Int -> String -> String
cjustify n x | n >= m     = space lm ++ x ++ space rm
             | otherwise  = error "cjustify failed because string is too long."
             where m :: Int
                   m = width x
                   lm :: Int
                   lm = (n - m) `div` 2
                   rm :: Int
                   rm = (n - m) - lm
--------------------------------------------------------------------------------
