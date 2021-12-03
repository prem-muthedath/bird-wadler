-- | chapter 2: bird & wadler, introduction to functional programming.
-- string data type: example.
-- usage: load this file in GHCi and invoke any of the top-level functions.
-- Prem Muthedath, 20 OCT 2021.

--------------------------------------------------------------------------------
module C2Strings where
-- | width of a given string.
width :: String -> Int
width [] = 0
width (_:xs) = 1 + width xs

-- | returns a string of space characters whose width is `n`.
space :: Int -> String
space 0 = ""
space n = " " ++ space (n-1)

-- | left-justifies a string.
ljustify :: Int -> String -> String
ljustify n x | n >= m    = x ++ space (n - m)
             | otherwise = error "ljustify failed because string is too long."
             where m :: Int
                   m = width x

-- | right-justifies a string.
rjustify :: Int -> String -> String
rjustify n x | n >= m     = space (n - m) ++ x
             | otherwise  = error "rjustify failed because string is too long."
             where m :: Int
                   m = width x

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
