-- | chapter 2: bird & wadler, introduction to functional programming.
-- char data type: examples.
-- usage: load this file in GHCi and invoke any of the top-level functions.
-- Prem Muthedath, 19 OCT 2021.

--------------------------------------------------------------------------------
module C2Char where
import Data.Char (chr, ord)

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
