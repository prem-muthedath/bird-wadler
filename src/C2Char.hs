-- | chapter 2: bird & wadler, introduction to functional programming.
-- char data type: examples.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler` to start GHCi.
--  3. next, at GHCi prompt, enter `import C2Char`.
--  4. you can then invoke any of the top-elevel functions.
-- Prem Muthedath, 19 OCT 2021.

--------------------------------------------------------------------------------
module C2Char where
--------------------------------------------------------------------------------
-- Data.Char @ https://tinyurl.com/2c72x8ya
import Data.Char (chr, ord)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | bird & wadler
--------------------------------------------------------------------------------
-- | `True` if character is a numeral.
isdigit :: Char -> Bool
isdigit x = (x >= '0') && (x <= '9')

-- | `True` for an uppercase character.
isupper :: Char -> Bool
isupper x = (x >= 'A') && (x <= 'Z')

-- | `True` for a lowercase character.
islower :: Char -> Bool
islower x = (x >= 'a') && (x <= 'z')

-- | capitalizes a lowercase English alphabet; others returned unchanged.
capitalize :: Char -> Char
capitalize x | islower x = chr (offset + ord x)
             | otherwise = x
             where offset :: Int
                   offset = ord 'A' - ord 'a'
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | unicode
--------------------------------------------------------------------------------
-- | convert a string of characters to unicode.
-- REF: /u/ pat, /u/ delta @ https://tinyurl.com/mpers6md (so)
-- NOTE: `Data.Char.ord` returns unicode code point associated with a character.
toUnicode :: String -> [Int]
toUnicode = map ord
--------------------------------------------------------------------------------
