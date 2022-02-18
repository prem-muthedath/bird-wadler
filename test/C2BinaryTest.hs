{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | quickcheck tests for ../src/C2Binary.hs
-- author: Prem Muthedath, JAN 2022.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler-test` to start GHCi.
--  3. at GHCi prompt, enter `import C2BinaryTest`.
--  4. you can then invoke `C2BinaryTest.ghciQC` to run all quickcheck tests.
--
--  REF: "A Guide to Writing Properties of Pure Functions", John Hughes.
--  https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf
--------------------------------------------------------------------------------
module C2BinaryTest where
--------------------------------------------------------------------------------
-- Test.QuickCheck @ https://tinyurl.com/2p9s9ets
-- Control.Monad @ https://tinyurl.com/mtvj95yx
-- Text.Read @ https://tinyurl.com/36etaccn
-- Numeric (base 4.14) @ https://tinyurl.com/3kvs3fha
-- Data.Char @ https://tinyurl.com/2c72x8ya
-- Data.Word @ https://tinyurl.com/2p8zph45
-- Data.Bits @ https://tinyurl.com/3b2bu6dt
-- Data.List @ https://tinyurl.com/ycxb9uaw
import Test.QuickCheck
import Control.Monad (liftM, liftM2)
import Text.Read (readMaybe)
import Numeric (readInt, showIntAtBase)
import Data.Char (isDigit, digitToInt, intToDigit, isAscii, chr, isSpace)
import Data.Word (Word8, Word16, Word64)
import Data.Bits ((.|.), shiftL)
import Data.List (isPrefixOf, isSuffixOf, stripPrefix, dropWhileEnd)

import C2Binary
import Common (ghciRunner)
--------------------------------------------------------------------------------
-- | quickcheck testing -- binary stuff.
--------------------------------------------------------------------------------
-- | common functions for number operations.
--------------------------------------------------------------------------------
-- | allowed `Int` upper bound.
-- maxBound :: Bounded a => a
-- NOTE: `Int` is 64 bits in GHC haskell, so we have:
--    `maxBound :: Int` = `(2 :: Int) ^ (63 :: Int) - 1` = 9223372036854775807.
upperInt :: Int
upperInt = maxBound :: Int

-- | convert string -> `Int`.
asInt :: String -> Int
asInt = read

-- | convert string -> `Integer`.
asInteger :: String -> Integer
asInteger = read

-- | `True` if string represents a valid number, positive or negative.
-- we use pattern guards. see /u/ melpomene @ https://tinyurl.com/3vfnxwa7 (so)
isNum :: String -> Bool
isNum s | ""          <- s = False
        | ('0':[])    <- s = True
        | ('0':_)     <- s = False
        | ('-':[])    <- s = False
        | ('-':'0':_) <- s = False
        | ('-':xs)    <- s = f xs
        | otherwise        = f s
        where f :: String -> Bool
              f = all isDigit

-- | `True` if string does not represent a valid number.
notNum :: String -> Bool
notNum = not . isNum
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | common types/functions for binary operations.
--------------------------------------------------------------------------------
-- | types for binary stuff.
--------------------------------------------------------------------------------
-- REF: `Binary`: /u/ nicolas, /u/ ankur @ https://tinyurl.com/2p93mvah (so).
-- REF: `Bit`: /u/ erikr @ https://tinyurl.com/2p87s5kv (so)
-- REF: `Num` defined in GHC.Num @ https://tinyurl.com/dkkx8j3y (hackage)
-- `Binary` implementation by Prem Muthedath.
newtype Binary = Binary (Bit, [Bit]) deriving Eq
data Bit = T | F deriving Eq

instance Enum Bit where
  fromEnum F = 0
  fromEnum T = 1
  toEnum n   = if even n then F else T

instance Num Bit where
  -- fromInteger :: Num a => Integer -> a
  -- (+) :: a -> a -> a infixl 6
  -- (*) :: a -> a -> a infixl 7
  -- negate :: Num a => a -> a
  -- signum :: Num a => a -> a
  -- NOTE: define one of these: x - y = x + negate y, negate x = 0 - x
  fromInteger = toEnum . fromInteger
  a + b       = toEnum (fromEnum a + fromEnum b)
  a * b       = toEnum (fromEnum a * fromEnum b)
  negate a    = a
  abs a       = a
  signum      = toEnum . signum . fromEnum

instance Show Bit where
  show T = "1"
  show F = "0"

instance Read Bit where
  -- readsPrec :: Read a => Int -> ReadS a
  readsPrec _ r = case r of
    ('0':xs)  -> return (toEnum 0, xs)
    ('1':xs)  -> return (toEnum 1, xs)
    _         -> []

instance Num Binary where
  -- fromInteger :: Num a => Integer -> a
  -- (+) :: a -> a -> a infixl 6
  -- (*) :: a -> a -> a infixl 7
  -- negate :: Num a => a -> a
  -- signum :: Num a => a -> a
  -- NOTE: define one of these: x - y = x + negate y, negate x = 0 - x
  fromInteger = \i -> asBinary $ (fromInteger i :: Integer)
  a + b       = addBin a b
                where addBin :: Binary -> Binary -> Binary
                      addBin x y = asBinary $
                        (fromBinary x :: Integer) + (fromBinary y :: Integer)
  a * b       = multBin a b
                where multBin :: Binary -> Binary -> Binary
                      multBin x y = asBinary $
                        (fromBinary x :: Integer) * (fromBinary y :: Integer)
  negate a    = a
  abs a       = a
  signum a    = if all (== F) (toBits a) then 0 else 1

instance Show Binary where
  show (Binary (x, xs))  = toBin $ map (intToDigit . fromEnum) (x:xs)

-- `Read` instance for `Binary`; author: Prem Muthedath.
instance Read Binary where
  -- type ReadS a = String -> [(a, String)]
  -- readsPrec :: Read a => Int -> ReadS a
  readsPrec _ r = do
    ("0", 'b':s:_) :: (String, String) <- lex r
    (a, "")        :: (Bit, String)    <- readsPrec 0 [s]
    (b, c)         :: ([Bit], String)  <- readBits $ drop 3 r
    return (Binary (a, b), c)
    where readBits :: ReadS [Bit]
          readBits "" = return ([], "")
          readBits s | nonBin (head s) = return ([], s)
                     | otherwise       = do
              (x, a)  :: (Bit, String)    <- readsPrec 11 s
              (y, b)  :: ([Bit], String)  <- readBits a
              return (x : y, b)

-- | alternative implementation of `readsPrec` for `Binary`.
-- implementation follows `++` pattern for `readsPrec` in haskell 2010 report.
-- i wrote it as an exercise to compare with `readsPrec` in `Read Binary`.
--
-- NOTE: the first call to `readsPrecBin` should have precedence < 11. as is 
-- custom, the first call should use 0 as precedence.
--
-- type ReadS a = String -> [(a, String)]
readsPrecBin :: Int -> ReadS Binary
readsPrecBin d0 x = if d0 < 11 then f x else g x
  where f :: ReadS Binary
        f r = [(p, q) |
                  ("0", 'b':s:_) :: (String, String) <- lex r, isBin s,
                  (p, q)         :: (Binary, String) <- readsPrecBin 11 (drop 2 r)
              ]
        g :: ReadS Binary
        g r = [(Binary (a, b), c) | (a:b, c) :: ([Bit], String)  <- readBits r]
          where readBits :: ReadS [Bit]
                readBits r1 = g1 r1 ++ g2 r1
                g1 :: ReadS [Bit]
                g1 ""                   = [([], "")]
                g1 r1 | nonBin (head r1)  = [([], r1)]
                      | otherwise        = []
                g2 :: ReadS [Bit]
                g2 r1 = [(a:c, d) |
                    (a, b)  :: (Bit, String)    <- readsPrec 11 r1,
                    (c, d)  :: ([Bit], String)  <- readBits  b
                  ]

-- | convert a `Binary` value to a `Num` instance value.
fromBinary :: Num a => Binary -> a
fromBinary b = fst . head . readBin $ show b

-- | `Binary` value as a `[Bit]`.
toBits :: Binary -> [Bit]
toBits (Binary (x, xs)) = x:xs

-- | makes a `Binary` value from a `[Bit]`; throws error if list is empty.
mkBinary :: [Bit] -> Binary
mkBinary []     = error "C2BinaryTest.mkBinary: [] supplied."
mkBinary (x:xs) = Binary (x, xs)

-- | number of bits in the `Binary` value.
binSize :: Binary -> Int
binSize = length . toBits

-- | convert an `Integral` value to `Binary`.
-- EXAMPLE: `254 :: Int` => `Binary (T, [T, T, T, T, T, T, F])`.
--
-- NOTE:
-- in essence, both `asBinary` and `readsPrec` for `Binary` do the same thing: 
-- they pasre the string to generate a `Binary`.  but `asBinary` code is 
-- different from, and simpler than, `readsPrec` for `Binary`, because over 
-- here, we do not need to worry about bad parses etc, since we start off with a 
-- valid number.  `readsPrec` has no such certainity: it has tp parse any 
-- string, good or or bad, so it's code is more complex.
--
-- showIntAtBase :: (Integral a, Show a) => a -> (Int -> Char) -> a -> ShowS
asBinary :: (Integral a, Show a) => a -> Binary
asBinary i | i >= 0     = mkBinary $ map (toEnum . digitToInt) showAsBinary
           | otherwise  = error msg
           where showAsBinary :: String
                 showAsBinary = showIntAtBase 2 intToDigit i ""
                 msg :: String
                 msg = "C2BinaryTest.asBinary: " <> show i <> " is < 0."

-- | convert a `Binary` to a "binary" decimal.
-- a "binary" decimal is really a binary literal of type `Int` or `Integer`.
-- EXAMPLE: `Binary (T, [T, F, F])` => 1100 :: Int`.
-- read :: Read a => String -> a
asBinDec :: (Integral a, Read a) => Binary -> a
asBinDec = read . fromBin . show
--------------------------------------------------------------------------------
-- | common functions for "binary" strings.
--------------------------------------------------------------------------------
-- | `True` if the string is binary.
-- 1. a binary string begins with "0b";
-- 2. &,all remaiining elements in the binary string are binary ('0's & '1's).
-- 3. string that has nothing but the "0b" prefix, just like "", is non-binary.
allBin :: String -> Bool
allBin []           = False
allBin ('0':'b':[]) = False
allBin ('0':'b':s)  = all isBin s
allBin  _           = False

-- | `True` if character is '0' or '1'.
isBin :: Char -> Bool
isBin = (`elem` ['0', '1'])

-- | `True` if the string is non-binary.
-- 1. string is non-binary if it does not begin with "0b";
-- 2. a "0b"-prefix string is non-binary if among its non-prefix elements we 
--    have >= 1 non-binary characters.
-- 3. string that has nothing but the "0b" prefix, just like "", is non-binary.
notBin :: String -> Bool
notBin ""           = True
notBin ('0':'b':[]) = True
notBin ('0':'b':s)  = any nonBin s
notBin _            = True

-- | `True` if the character is non-binary".
nonBin :: Char -> Bool
nonBin = not . isBin

-- | prefix "0b" to a string of 1s & 0s.
toBin :: String -> String
toBin ""              = ""
toBin s | all isBin s = '0':'b':s
        | otherwise   = s

-- | remove "0b" prefix of a binary string.
fromBin :: String -> String
fromBin s | allBin s  = drop 2 s
          | otherwise = s

-- | drop leading zeros from a binary string.
-- returns `Nothing` if string is non-binary, even if it has a binary prefix.
dropLeading0s :: String -> Maybe String
dropLeading0s bin | notBin bin     = Nothing
                  | all (== '0') s = Just $ toBin "0"
                  | otherwise      = Just $ toBin $ dropWhile (== '0') s
                  where s :: String = fromBin bin

-- | reads a "binary" string, returning the number it represents in decimal.
-- 1. "binary" string must begin with "0b".
-- 2. empty string ("", "0b") & non-binary string throw error. non-binary string 
--    includes anything that begins with '-'; i.e., -ve numbers are NOT allowed.
-- 3. example: `readBin "0b11111111" :: [(Int, String)] = [(255, "")]`
readBin :: Num a => ReadS a
-- readInt :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readBin bin | good      = readInt 2 f g (fromBin bin)
            | otherwise = error $ "C2BinaryTest.readBin => "
                                     <> "bad binary: " <> bin
  where good :: Bool            = allBin bin
        f    :: (Char -> Bool)  = isBin
        g    :: (Char -> Int)   = Data.Char.digitToInt
--------------------------------------------------------------------------------
-- | common functions to determine if a number is binary or not.
--------------------------------------------------------------------------------
-- NOTE: some numbers, such as 1100, can be binary as well as decimal.  so how 
-- do we distinguish true binary from decimal?  well, we have created a `Binary` 
-- type that is guaranteed to be a binary number, and its string literal begins 
-- with a "0b", which is not so with non-binary numbers.  so given a number, we 
-- can use its string literal to determine if it is a binary or not.

-- | `True` if the number, an instance of `Num`, is "binary".
binNum :: (Num a, Show a) => a -> Bool
binNum = allBin . show

-- | `True` if number is non-binary, i.e., a decimal.
decimal :: (Num a, Show a) => a -> Bool
decimal = notBin . show
--------------------------------------------------------------------------------
-- | common utility functions
--------------------------------------------------------------------------------
-- | trim leading & trailing white space characters, including '\n', etc.
-- REF: /u/ spopejoy @ https://tinyurl.com/2aed54ax (so)
trim' :: String -> String
trim' = dropWhileEnd isSpace . dropWhile isSpace
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | generators.
--------------------------------------------------------------------------------
-- | generate binary & non-binary test cases for testing `isBin` & `nonBin`.
genBitTestCases :: Gen (Char, Char)
genBitTestCases = do
  bin    <- elements ['0', '1']
  nonbin <- (arbitrary :: Gen Char) `suchThat` (not . (`elem` ['0', '1']))
  return (bin, nonbin)

-- | generate binary & non-binary strings to test common functions like 
-- `allBin`, `notBin`, toBin, `fromBin`:
genBinStrTestCases :: Gen (String, String, String, String, String)
genBinStrTestCases = do
  bin     <- ("0b" ++) <$> listOf1 ((arbitrary :: Gen Char) `suchThat` isBin)
  nonbin1 <- return $ drop 2 bin
  nonbin2 <- elements ["", "0b"]
  nonbin3 <- listOf (arbitrary :: Gen Char) `suchThat` (any nonBin)
  nonbin4 <- return ("0b" ++ nonbin3)
  return (bin, nonbin1, nonbin2, nonbin3, nonbin4)

-- | generate test cases for testing `decimal`, `binNum`.
genBinNumTestCase :: Gen (Binary, Integer)
genBinNumTestCase = do
  bin :: Binary  <- asBinary <$> (arbitrary :: Gen Integer) `suchThat` (>= 0)
  dec :: Integer <- (arbitrary :: Gen Integer) `suchThat` (>= 0)
  return (bin, dec)
--------------------------------------------------------------------------------
-- | generate `Int` string, with values in range 0 .. 9223372036854775807.
-- 1. `Int` strings represent positive numbers, and they are non-empty.
-- 2. because of the use of `readMaybe` in `intStrToBinStr`, strings with space 
--    characters as prefix or siffix pass for numbers, so we include those 
--    conditions in this generator.
-- 3. NOTE: as documentation, the 1st version (very crude!) listed below.
--    genIntStr = do
--      x <- (arbitrary :: Gen Char)
--           `suchThat`
--           (\x -> x `elem` digits && x /= '0')
--      xs <- listOf ((arbitrary :: Gen Char)
--                    `suchThat`
--                    (`elem` digits))
--            `suchThat`
--            (\ys -> length ys < 9)
--      return (x:xs)
--      where digits :: String = "0123456789"
--  EXAMPLES: "10", "1", "17", "\r9\n".
genIntStr :: Gen String
genIntStr = do
  spac  :: String <- listOf1 $ (arbitrary :: Gen Char) `suchThat` isSpace
  edge  :: Int    <- elements [0, upperInt]
  inner :: Int    <- (arbitrary :: Gen Int)
                     `suchThat`
                     (\x -> x > 0 && x < upperInt)
  mix   :: String <- concat <$> shuffle [show inner, spac]
  frequency [ (1, return $ show edge)
            , (3, return mix)
            , (20, return $ show inner)
            ]
--------------------------------------------------------------------------------
-- | generate a non-digit character. non-digit means not any in '0' .. '9'.
-- EXAMPLES: '[', '\1103141'.
genNonDigit :: Gen Char
genNonDigit = (arbitrary :: Gen Char) `suchThat` (not . isDigit)
--------------------------------------------------------------------------------
-- | generate "bad" `Int` string.
-- "bad Int" strings can:
--    1. be empty,
--    2. contain non-digits,
--    3. represent negative numbers or numbers beyond `Int` upper limit,
--    4. or contain a mix of digits & non-digits.
--    5. be pure numbers, but they will NEVER have leading/trailing spaces.
--    EXAMPLES: "-2", "", "9223372036854775845", "1/lk", "\SI;\SO".
genBadIntStr :: Gen String
genBadIntStr = do
  str :: String <- listOf genNonDigit
  neg :: String <- show <$> ((arbitrary :: Gen Int)
                            `suchThat`
                            (< 0))
  big :: String <- show <$>
          -- fromIntegral :: (Integral a, Num b) => a -> b
          chooseInteger ( (fromIntegral upperInt :: Integer) + 1
                        , (fromIntegral upperInt :: Integer) + 90
                        )
  mix :: String <- listOf1 ((arbitrary :: Gen Char))
                  `suchThat`
                  (\xs -> any (not . isDigit) xs &&
                          any isDigit xs &&
                          trim' xs == xs) -- eliminate leading/trailing spaces
  frequency [ (1, return str)
            , (1, return neg)
            , (1, return big)
            , (2, return mix)
            ]
--------------------------------------------------------------------------------
-- | generate a non-number.
-- EXAMPLES: "", "00815","-00", "\141574\63324&e+Z!",
genNonNum :: Gen String
genNonNum = do
  pre :: String <- listOf1 (elements "-0")
                  `suchThat`
                  -- make sure we return a string of at least length 2. this 
                  -- will ensure that we never generate a "0", a valid number, 
                  -- but we can expect to generate a "00", a non-number.
                  -- "< 64" ensures that we stay within `Int` range.
                  (\xs -> length xs > 1 && length xs < 64)
  num :: String <- listOf ((arbitrary :: Gen Char) `suchThat` isDigit)
                  `suchThat`
                  -- "< 64" ensures that we stay within `Int` range.
                  (\xs -> length xs < 64)
  str :: String <- listOf genNonDigit
                  `suchThat`
                  -- "< 64" ensures that we stay within `Int` range.
                  (\xs -> length xs < 64)
  frequency [ (1, return $ pre ++ num) -- returns, on purpose, string of size >= 2.
            , (2, return str)
            ]
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `Bit`.
instance Arbitrary Bit where
  arbitrary = elements [T, F]
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `Binary`.
instance Arbitrary Binary where
  -- Control.Monad.liftM :: Monad m => (a1 -> r) -> m a1 -> m r
  -- Control.Monad.liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
  -- listOf :: Gen a -> Gen [a]
  arbitrary = liftM Binary $
                liftM2 (,)
                       (arbitrary :: Gen Bit)
                       (listOf (arbitrary :: Gen Bit))
--------------------------------------------------------------------------------
-- | generate a data structure that represents a mix of `Binary` & non-`Binary`.
-- the (`Binary`, String) tuple represents this data structure. the `Binary` in 
-- the pair forms the string's prefix and suffix; the infix part of the string 
-- is non-`Binary`, though it may contain '1's & '0's.
-- EXAMPLES: (0b0,"0b0\39751\&0b0"), (0b1,"0b1\EOT0b1").
genMixedBinary :: Gen (Binary, String)
genMixedBinary = do
  bin :: Binary  <- arbitrary
  str :: String  <- listOf1 (arbitrary :: Gen Char)
                    `suchThat`
                    (nonBin . head)
  return (bin, show bin ++ str ++ show bin)
--------------------------------------------------------------------------------
-- | generate a string that represents a non-`Binary`.
-- NOTE: the string generated here will never have as prefix any sequence of 
-- characters that can be considered `Binary`, so, for example, you'll never 
-- have "0b0prem". `genBadBinaryStr`, on the other hand, allows `Binary` 
-- sub-sequences as prefix as long as the string as a whole is non-binary.
-- EXAMPLES: "\22728\1034044!1\RS", "0b\1070673P", "", "B".
genNonBinary :: Gen String
genNonBinary = do
  x1 <- elements ["", "0b"]
  x2 <- listOf1 (arbitrary :: Gen Char) `suchThat` (nonBin . head)
  x3 <- listOf1 (arbitrary :: Gen Char) `suchThat` notBin
  frequency [ (1, return x1)
            , (2, return $ x1 ++ x2)
            , (2, return x2)
            , (2, return x3)
            ]
--------------------------------------------------------------------------------
-- | generate a "binary" string.
-- 1. generated strings ALWAYS START with "0b".
-- 2. generated strings, after excluding the "0b" prefix, are ALWAYS NON-EMPTY.
-- EXAMPLES; "0b0", "0b01100011011", "0b100".
genBinaryStr :: Gen String
genBinaryStr = show <$> (arbitrary :: Gen Binary)
--------------------------------------------------------------------------------
-- | generate binary string < 64 in length (i.e., within `Int` range).
-- generated binary string:
-- 1. begins with "0b";
-- 2. is ALWAYS NON-EMPTY: i.e., /= "0b" or "";
-- 3. has < 64 characters in the part after the "0b" prefix.
-- EXAMPLES: "0b1", "0b001111", "0b011001100011100".
--
-- NOTE: `Int` has 64 bits, but the leftmost bit is a sign bit, so 63 bits, not 
-- 64, determine the upper size limit, because if we have 63 '1's, we reach 
-- `Int`s maxBound, as shown below:
-- "111111111111111111111111111111111111111111111111111111111111111" is = 
-- (maxBound :: Int) = 9223372036854775807 = (2 :: Int) ^ (63 :: Int) - 1.
-- this is because, mathematically, 2^63 - 1 = 2 ^ 62 + 2 ^ 61 + ... + 2 ^ 0.
-- for proof, see /u/ parcly taxel @ https://tinyurl.com/4d29wmr8 (math.SE)
genBinaryStr64 :: Gen String
genBinaryStr64 = show <$> ((arbitrary :: Gen Binary)
                           `suchThat`
                           (\bin -> binSize bin < 64))
--------------------------------------------------------------------------------
-- | generate "bad binary" string.
-- "bad binary" string:
-- 1. may or may not begin with "0b";
-- 2. can be EMPTY;
-- 3. may have binary characters '0' & '1';
-- 4. is assured to have 1 or more non-binary characters
-- 5. may have leading/trailing white spaces, including stuff like "\n\t".
-- EXAMPLES; "", "2\NAKH", "6", "f", "0b\1053986aSsm7 bL\128707", "+1\151779".
genBadBinaryStr :: Gen String
genBadBinaryStr = do
    pre    <- elements ["", "0b"]
    nonbin <- listOf (arbitrary :: Gen Char)
              `suchThat`
              (any nonBin)
    frequency [ (1,  return pre)
              , (2,  return $ pre ++ nonbin)
              , (10, return nonbin)
              ]
--------------------------------------------------------------------------------
-- | generate "bad binary" string with length < 64 (within `Int` range).
-- same as `genBadBinaryStr`, except that the generated string size is < 64.
-- EXAMPLES: " ", "", "J", "1\1096197".
--
-- NOTE: `Int` has 64 bits, but the leftmost bit is a sign bit, so 63 bits, not 
-- 64, determine the upper size limit, because if we have 63 '1's, we reach 
-- `Int`s maxBound, as shown below:
-- "111111111111111111111111111111111111111111111111111111111111111" is = 
-- (maxBound :: Int) = 9223372036854775807 = (2 :: Int) ^ (63 :: Int) - 1.
-- this is because, mathematically, 2^63 - 1 = 2 ^ 62 + 2 ^ 61 + ... + 2 ^ 0.
-- for proof, see /u/ parcly taxel @ https://tinyurl.com/4d29wmr8 (math.SE)
genBadBinaryStr64 :: Gen String
genBadBinaryStr64 = genBadBinaryStr `suchThat` (\xs -> length xs < 64)
--------------------------------------------------------------------------------
-- | generate "binary" decimal.
-- 1. a "binary" decimal is really a binary literal of type `Int` or `Integer`.
-- 2. `Binary (x, xs)` "represents" a binary decimal, because we can easily 
--    translate it to a binary literal.
-- 3. binary decimals represent `Int` values >= 0; in fact, we generate binary 
--    decimals from `Int` values >= 0.
-- EXAMPLES: `Binary (F, [])`, `Binary (T, [])`, `Binary (T, [F])`.
genBinDec :: Gen Binary
genBinDec = do
  x :: Int <- (arbitrary :: Gen Int) `suchThat` (>= 0)
  return $ asBinary x
--------------------------------------------------------------------------------
-- | generate "bad binary" decimal.
-- "bad binary decimal" is an `Int` that has at least 1 non-binary character.
-- EXAMPLES: -1, -13, -10, 4.
genBadBinDec :: Gen Int
genBadBinDec = (arbitrary :: Gen Int)
               `suchThat`
               (\x -> any nonBin $ show x)
------------------------------------------`--------------------------------------
------------------------------------------`--------------------------------------
-- | test the basic common functions & their generators!
--------------------------------------------------------------------------------
-- | check "bit" test cases generation.
prop_genBitTestCases :: Property
prop_genBitTestCases = forAll genBitTestCases $
  \(bin, nonbin) -> (bin `elem` ['0', '1']) .&&. (not (nonbin `elem` ['0', '1']))
--------------------------------------------------------------------------------
-- | check "binary" string test cases generation.
prop_genBinStrTestCases :: Property
prop_genBinStrTestCases = forAll genBinStrTestCases $
  \(x1, x2, x3, x4, x5) -> (x1 === '0':'b':x2) .&&.
                           (x2 =/= "" .&&. all (`elem` ['0', '1']) x2) .&&.
                           (x3 `elem` ["", "0b"]) .&&.
                           (any (not . (`elem` ['0', '1'])) x4) .&&.
                           (x5 === '0':'b':x4)
--------------------------------------------------------------------------------
-- | check "binary" number test cases generation.
prop_genBinNumTestCases :: Property
prop_genBinNumTestCases = forAll genBinNumTestCase $
  \(bin, dec) -> let x1 = show bin
                     x2 = drop 2 x1
                     x3 = show dec
                 in (x1 === '0':'b':x2) .&&.
                    (x2 =/= "" .&&. x3 =/= "") .&&.
                    (all (`elem` ['0', '1']) x2) .&&.
                    (all isDigit x3)
--------------------------------------------------------------------------------
-- | check if we are generating `Bit` correctly.
prop_arbitraryBit :: Property
prop_arbitraryBit = forAll (arbitrary :: Gen Bit) $
  \x  -> classify (x == T) "T" $
         classify (x == F) "F" $
         (x === T) .||. (x === F)
--------------------------------------------------------------------------------
-- | check if we are generating `Binary` correctly.
prop_arbitraryBinary :: Property
prop_arbitraryBinary = forAll (arbitrary :: Gen Binary) $
  \(Binary (x, xs)) -> all (`elem` [T, F]) (x:xs)
--------------------------------------------------------------------------------
-- | check if `isBin` does what is expected.
prop_isBin :: Property
prop_isBin = forAll genBitTestCases $
  \(x1, x2) -> map isBin [x1, x2] === [True, False]

-- | check if `nonBin` does what is expected.
prop_nonBin :: Property
prop_nonBin = forAll genBitTestCases $
  \(x1, x2) -> map nonBin [x1, x2] === [False, True]

-- | check if `allBin` does what is expected.
prop_allBin :: Property
prop_allBin = forAll genBinStrTestCases $
  \(x1, x2, x3, x4, x5) -> map allBin [x1, x2, x3, x4, x5]
                           === [True, False, False, False, False]

-- | check if `notBin` does what is expected.
prop_notBin :: Property
prop_notBin = forAll genBinStrTestCases $
  \(x1, x2, x3, x4, x5) -> map notBin [x1, x2, x3, x4, x5]
                           === [False, True, True, True, True]

-- | check if `toBin` does what is expected.
prop_toBin :: Property
prop_toBin = forAll genBinStrTestCases $
  \(x1, x2, x3, x4, x5) -> map toBin [x1, x2, x3, x4, x5]
                           === [x1, '0':'b':x2, x3, x4, x5]

-- | check if `fromBin` does what is expected.
prop_fromBin :: Property
prop_fromBin = forAll genBinStrTestCases $
  \(x1, x2, x3, x4, x5) -> map fromBin [x1, x2, x3, x4, x5]
                           === [drop 2 x1, x2, x3, x4, x5]

-- | check if `binNum` does what is expected.
prop_binNum :: Property
prop_binNum = forAll genBinNumTestCase $
  \(bin, dec) -> (binNum bin === True) .&&. (binNum dec === False)

-- check if `decimal` does what is expected.
prop_decimal :: Property
prop_decimal = forAll genBinNumTestCase $
  \(bin, dec) -> (decimal bin === False) .&&. (decimal dec === True)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | test the other generators!
--------------------------------------------------------------------------------
-- | check if generated `mixed` `Binary` is valid.
prop_genMixedBinary :: Property
prop_genMixedBinary = forAll genMixedBinary $
  \(bin, mix) -> let bins   = show bin
                     binPre = isPrefixOf bins mix
                     binSuf = isSuffixOf bins mix
                 in classify binPre "has binary prefix" $
                    classify binSuf "has binary suffix" $
                    classify (notBin mix) "not binary string" $
                    binNum bin .&&.
                    binPre .&&.
                    binSuf .&&.
                    case stripPrefix bins mix of
                      Just (x:_)  -> property $ nonBin x
                      _           -> property False
--------------------------------------------------------------------------------
-- | check if generated "non-binary" is valid.
prop_genNonBinary :: Property
prop_genNonBinary = forAll genNonBinary $
  \nonbin -> classify (nonbin == "") "empty" $
             classify (nonbin == "0b") "= 0b" $
             classify (isPrefixOf "0b" nonbin) "start with \"0b\"" $
             classify (notBin nonbin) "non-binary string" $
             case nonbin of
                ""            -> property True
                "0b"          -> property True
                ('0':'b':s:_) -> property $ nonBin s
                _             -> property $ notBin nonbin
--------------------------------------------------------------------------------
-- | check if generated `Int` string is valid.
prop_genIntStr :: Property
prop_genIntStr = forAll genIntStr $
  \x -> classify (isNum x) "number" $
        classify (asInt x == 0) "= 0" $
        classify (asInt x > 0 && asInt x < upperInt) "in 1 .. 2 ^ 63 - 2" $
        classify (asInt x == upperInt) "= 2 ^ 63 - 1" $
        classify (trim' x /= x) "contain leading/trailing spaces" $
        (x =/= "") .&&. (asInt x >= 0 .&&. asInt x <= upperInt)
        .&&. case isNum x of
              True  -> trim' x === x
              False -> trim' x =/= x
--------------------------------------------------------------------------------
-- | check if generated "bad" `Int` string is indeed bad.
-- fromIntegral :: (Integral a, Num b) => a -> b
prop_genBadIntStr :: Property
prop_genBadIntStr = forAll genBadIntStr $
  \x -> classify (x == "") "empty" $
        classify (notNum x && trim' x /= x) "non-numbers have leading/trailing spaces" $
        classify (isNum x && trim' x == x) "numbers with NO leading/trailing spaces" $
        classify (isNum x) "number" $
        classify (isNum x && asInteger x < 0) "< 0" $
        classify (isNum x &&
          asInteger x > (fromIntegral upperInt :: Integer)) "> 2 ^ 63 - 1" $
        classify (notNum x) "non-number" $
        classify (notNum x && any isDigit x) "non-number with >= 1 digits" $
        check x
  where check :: String -> Property
        check x | x == ""   = property True
                | isNum x   = asInteger x < 0 .||.
                              asInteger x > (fromIntegral upperInt :: Integer)
                | notNum x  = property True
                | otherwise = property False
--------------------------------------------------------------------------------
-- | check if generated "non-number" is indeed a non-number.
-- NOTE: we do not have a similiar test for a number generator, because it is so 
-- simple: `arbitrary :: Gen Int`.
--
-- we use `readMaybe` as our "model", but `readMaybe "0023" :: Int` returns 23, 
-- but over here, we want it to return `Nothing`, because we consider 0023 as a 
-- non-number.  likewise, `readMaybe "-023" :: Int` returns -23, but, again, we 
-- want it to return `Nothing` here, because -023 is a non-number for us.
--
-- so clearly there is a mismatch with what we want and what our model gives, 
-- and this mismatch ONLY occurs when we have leading zeroes.  in such cases, 
-- `readMaybe` ignores the leading zeroes and reads the rest. for all other 
-- cases, what `readMaybe` returns matches what we define as a non-number.
--
-- so to use `readMaybe` as our model, in cases when `readMaybe` succeeds, we do 
-- a length comparision of the original string with the string representation of 
-- the number `readMaybe` returns. in case of leading zeroes, this means that 
-- the read value will always be shorter than the original. this seems to work! 
--
-- NOTE: we use `>`, instead of `>=`, because all our values are expected to be 
-- non-numbers. had this not been the case, we would have used `>=`.
--
-- NOTE: also, when we have numbers, `genNonNUm` string always returns a string 
-- of at least size 2. this is done on purpose, so that we never get a "0", 
-- which is a valid number, but we can expect to find "00", a non-number.
prop_genNonNum :: Property
prop_genNonNum = forAll genNonNum $
  \s  -> classify (s == []) "empty" $
         classify (startsWith00 s) "start with 00" $
         classify (startsWithMinus s) "start with -" $
         classify (startsWithMinus0 s) "start with -0" $
         -- readMaybe :: Read a => String -> Maybe a
         case readMaybe s :: Maybe Int of
            Just num  -> if length s > length (show num)
                            then property True
                            else property False
            Nothing   -> property True
  where startsWith00 :: String -> Bool
        startsWith00 []      = False
        startsWith00 (_:[])  = False
        startsWith00 (x:y:_) = x == '0' && y == '0'
        startsWithMinus :: String -> Bool
        startsWithMinus []     = False
        startsWithMinus (x:_)  = x == '-'
        startsWithMinus0 :: String -> Bool
        startsWithMinus0 []      = False
        startsWithMinus0 (_:[])  = False
        startsWithMinus0 (x:y:_) = x == '-' && y == '0'
--------------------------------------------------------------------------------
-- | check if generated "binary" string is valid.
prop_genBinaryStr :: Property
prop_genBinaryStr = forAll genBinaryStr $
  \bin -> let xs = fromBin bin in
          classify (allBin bin) "binary" $
          classify (bin /= "") "non-empty" $
          classify (xs /= "0b") "/= \"0b\"" $
          classify (bin /= xs && head xs == '0') "start with 0b0" $
          classify (bin /= xs && head xs == '1') "start with 0b1" $
          classify (length xs > 64) "size > 64" $
          allBin bin
--------------------------------------------------------------------------------
-- | check if generated "binary" string `64` is valid.
prop_genBinaryStr64 :: Property
prop_genBinaryStr64 = forAll genBinaryStr64 $
  \bin -> let xs = fromBin bin in
          classify (allBin bin) "binary" $
          classify (bin /= "") "non-empty" $
          classify (xs /= "0b") "/= \"0b\"" $
          classify (bin /= xs && head xs == '0') "start with 0b0" $
          classify (bin /= xs && head xs == '1') "start with 0b1" $
          classify (length xs < 64) "size < 64" $
          length xs < 64 .&&. allBin bin
--------------------------------------------------------------------------------
-- | check if generated "bad binary" is indeed non-binary.
prop_genBadBinaryStr :: Property
prop_genBadBinaryStr = forAll genBadBinaryStr $
  \xs -> classify (notBin xs) "non-binary" $
         classify (xs == "") "empty" $
         classify (trim' xs /= xs) "contain leading/trailing spaces" $
         classify (xs == "0b") "= \"0b\"" $
         classify (length xs > 2 && take 2 xs == "0b") "begin with \"0b\"" $
         classify (any (== '0') xs) "has 0" $
         classify (any (== '1') xs) "has 1" $
         classify (any (== '9') xs) "has 9" $
         classify (length xs > 64) "size > 64" $
         notBin xs
--------------------------------------------------------------------------------
-- | check if generated "bad binary" `64` is indeed non-binary.
prop_genBadBinaryStr64 :: Property
prop_genBadBinaryStr64 = forAll genBadBinaryStr64 $
  \xs -> classify (notBin xs) "non-binary" $
         classify (xs == "") "empty" $
         classify (trim' xs /= xs) "contain leading/trailing spaces" $
         classify (xs == "0b") "= \"0b\"" $
         classify (length xs > 2 && take 2 xs == "0b") "begin with \"0b\"" $
         classify (any (== '0') xs) "has 0" $
         classify (any (== '1') xs) "has 1" $
         classify (any (== '9') xs) "has 9" $
         classify (length xs < 64) "size < 64" $
         length xs < 64 .&&. notBin xs
--------------------------------------------------------------------------------
-- | check if generated "binary" decimal is valid: >= 0 and has only 1s & 0s.
prop_genBinDec :: Property
prop_genBinDec = forAll genBinDec $
  \bin -> classify (binNum bin) "binary" $
          let num :: Int = asBinDec bin
          in classify (num == 0) "= 0" $
             classify (num > 0) "> 0" $
             binNum bin .&&. property (num >= 0)
--------------------------------------------------------------------------------
-- | check if generated "bad" binary decimal is indeed non-binary.
prop_genBadBinDec :: Property
prop_genBadBinDec = forAll genBadBinDec $
  \bad -> classify (bad < 0) "< 0" $
          classify (decimal bad) "non-binary" $
          decimal bad
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | properties -- test common/supporting functions.
--------------------------------------------------------------------------------
-- | check `Bit` read.
prop_readBit :: Property
prop_readBit = forAll (arbitrary :: Gen Bit) $
  \b -> classify (b == T) "T" $
        classify (b == F) "F" $
        (b, "") `elem` (readsPrec 0 (showsPrec 0 b ""))
--------------------------------------------------------------------------------
-- | check `Bit` read on a string with a `Binary` prefix & non-binary infix.
prop_readMixedBit :: Property
prop_readMixedBit = forAll genMixedBinary $
  \(_, mix) -> classify ("0b" `isPrefixOf` mix) "begin with \"0b\"" $
               classify (notBin mix) "not binary" $
               (readsPrec 0 mix :: [(Bit, String)]) === [(F, tail mix)]
--------------------------------------------------------------------------------
-- | check `Bit` read on non-`Binary` string.
prop_readNonBit :: Property
prop_readNonBit = forAll genBad $
  \bad -> (readsPrec 0 bad :: [(Bit, String)]) === []
  where genBad :: Gen String
        genBad = listOf (arbitrary :: Gen Char)
                 `suchThat`
                 (\xs -> xs == "" || (nonBin . head $ xs))
--------------------------------------------------------------------------------
-- | check `Binary` read.
prop_readBinary :: Property
prop_readBinary = forAll (arbitrary :: Gen Binary) $
  \bin -> (bin, "") `elem` (readsPrec 0 (showsPrec 0 bin ""))
--------------------------------------------------------------------------------
-- | check `Binary` read on s string that has a mix of `Binary` & non-binary.
prop_readMixedBinary :: Property
prop_readMixedBinary = forAll genMixedBinary $
  \(bin, mix) -> classify (isBin . head . drop 2 $ mix) "have binary prefix" $
                 classify (notBin mix) "non-binary" $
                 let x :: [(Binary, String)] = readsPrec 0 mix
                 in case x of
                      ((a, b) : [])  -> (a === bin) .&&. ((show a) ++ b) === mix
                      _              -> property False
--------------------------------------------------------------------------------
-- | check `Binary` read on non-`Binary` string.
prop_readNonBinary :: Property
prop_readNonBinary = forAll genNonBinary $
  \bad -> (readsPrec 0 bad :: [(Binary, String)]) === []
--------------------------------------------------------------------------------
-- | check `readsPrecBin` `Binary` read.
prop_readsPrecBin :: Property
prop_readsPrecBin = forAll (arbitrary :: Gen Binary) $
  \bin -> (bin, "") `elem` (readsPrecBin 0 (showsPrec 0 bin ""))
--------------------------------------------------------------------------------
-- | test `readsPrecBin` on string that has mix of `Binary` & non-Binary.
prop_readsPrecBinMixed :: Property
prop_readsPrecBinMixed = forAll genMixedBinary $
  \(_, mix) -> let x :: [(Binary, String)] = readsPrec 0 mix
               in readsPrecBin 0 mix === x
--------------------------------------------------------------------------------
-- | check `readsPrecBin` on non-`Binary` string.
prop_readsPrecBinNonBinary :: Property
prop_readsPrecBinNonBinary = forAll genNonBinary $
  \bad -> let x :: [(Binary, String)] = readsPrec 0 bad
              y :: [(Binary, String)] = readsPrecBin 0 bad
          in (y === []) .&&. (x === y)
--------------------------------------------------------------------------------
-- | check if `asBinary` works as expected.
-- REF: see /u/ fp_mora, /u/ dave4420 @ https://tinyurl.com/2p9cjft5 (so)
-- REF: see `iterate` in Data.List @ https://tinyurl.com/ycxb9uaw
-- iterate :: (a -> a) -> a -> [a]
-- iterate f x =  x : iterate f (f x)
-- iterate f x == [x, f x, f (f x), ...]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
prop_asBinary :: Property
prop_asBinary = forAll ((arbitrary :: Gen Int) `suchThat` (>= 0)) $
  \int  -> classify (int == 0) "= 0" $
           classify (int > 0) "> 0" $
           let x :: [Bit]  = toBits $ asBinary int
               y :: [Int]  = map fromEnum (reverse x)
               f :: [Int]  = iterate (* 2) (1 :: Int)
           in int === (sum $ zipWith (*) y f)
--------------------------------------------------------------------------------
-- | check if `asBinary` fails as expected for negative numbers.
-- NOTE: `/=` returns `Bool`, which avoids `Exception thrown while showing test 
-- case` message that pops when we use  `=/=`, as it returns `Property`.
prop_asBinaryFail :: Property
prop_asBinaryFail = expectFailure $
  forAll ((arbitrary :: Gen Int) `suchThat` (< 0)) $
    \int  -> (toBits $ asBinary int) /= []
--------------------------------------------------------------------------------
-- | check if `asBinDec` works as expected.
-- NOTE: `from /u/ geekosaur @ haskell irc:
-- @src Maybe fail: fail _ = Nothing`, so in the `Maybe` monad, "giving up" is 
-- the definition of `fail`. this is why quickcheck reports "gave up", instead 
-- of `fail`, in the `Maybe` monad whenever the `do` block returns `Nothing`.
prop_asBinDec :: Property
prop_asBinDec = forAll (arbitrary :: Gen Binary) $
  \bin -> do let x1 = toBin . show $ (asBinDec bin :: Integer)
             x2 :: String <- dropLeading0s . show $ bin
             return $ x1 === x2
--------------------------------------------------------------------------------
-- | check if `readBin` does what is expected.
prop_readBin :: Property
prop_readBin = forAll genBinaryStr64 $
  \bin -> case readBin bin :: [(Int, String)] of
            ((x, _) : []) -> Just (show . asBinary $ x) === dropLeading0s bin
            _             -> property False
--------------------------------------------------------------------------------
-- | check if `readBin` fails as expected for mixed binary input.
-- NOTE: `readBin` will fail for any non-binary string, including strings that 
-- have a binary prefix. `readBin`, when it works, will always be a full parse.
-- NOTE: `/=` returns `Bool`, which avoids `Exception thrown while showing test 
-- case` message that pops when we use  `=/=`, as it returns `Property`.
prop_readBinMixed :: Property
prop_readBinMixed = expectFailure $ forAll genMixedBinary $
  \(_, mix) -> (readBin mix ::[(Integer, String)]) /= []
--------------------------------------------------------------------------------
-- | check if `readBin` fails as expected for non-binary input.
-- NOTE: `/=` returns `Bool`, which avoids `Exception thrown while showing test 
-- case` message that pops when we use  `=/=`, as it returns `Property`.
prop_readBinBad :: Property
prop_readBinBad = expectFailure $ forAll genBadBinaryStr64 $
  \bad  -> (readBin bad :: [(Int, String)]) /= []
--------------------------------------------------------------------------------
-- | check if `isNum` works for both numbers as well as non-numbers.
-- check for numbers.
prop_isNum :: Property
prop_isNum = forAll (arbitrary :: Gen Int) $
  \num  -> classify (num == 0) "0" $
           classify (num < 0) "< 0" $
           classify (num > 0) "> 0" $
           isNum $ show num

-- check for non-numbers.
prop_notIsNum :: Property
prop_notIsNum = forAll genNonNum $ not . isNum
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | properties -- test main functions.
--------------------------------------------------------------------------------
-- | check `Int` string -> "binary" string conversion.
prop_intStrToBinStr :: Property
prop_intStrToBinStr = forAll genIntStr $
  \x -> classify (isNum x) "number" $
        classify (trim' x /= x) "contain leading/trailing spaces" $
        classify (x == "0") "= 0" $
        classify (x == show upperInt) "= 2^63 - 1" $
        case intStrToBinStr x of
            Left _    -> property False
            Right bin -> check x bin
  where check :: String -> String -> Property
        check x bin = r1 === r2
          where r1 :: [(Int, String)] = trimTL $ readsPrec 0 x
                r2 :: [(Int, String)] = trimTL $ readBin (toBin bin)
                trimTL :: [(Int, String)] -> [(Int, String)]
                trimTL = map (\(a, b) -> (a, trim' b))
--------------------------------------------------------------------------------
-- | check "bad" `Int` string -> "binary" string conversion.
prop_badIntStrToBinStr :: Property
prop_badIntStrToBinStr = forAll genBadIntStr $
  \x -> classify (x == "") "empty" $
        classify (notNum x) "non-number" $
        classify (isNum x) "number" $
        classify (notNum x && trim' x /= x) "non-numbers have leading/trailing spaces" $
        classify (isNum x && trim' x == x) "numbers with NO leading/trailing spaces" $
        case intStrToBinStr x of
            Left _  -> property True
            Right _ -> property False
--------------------------------------------------------------------------------
-- | check `Word64` -> `[Word64]` binary, "decimal" -> "bits" conversions.
prop_toBinary :: Property
prop_toBinary = forAll genWord64 $
  \x -> classify (x == 0) "= 0" $
        classify (x > 0) "> 0" $
        toBinary x === decToBits x
  where genWord64 :: Gen Word64
        genWord64 = (arbitrary :: Gen Word64) `suchThat` (>= 0)
--------------------------------------------------------------------------------
-- | check `expectFailure` in "decimal" -> "bits" conversion.
prop_decToBitsFailure :: Property
prop_decToBitsFailure = expectFailure $
  -- NOTE: `/=` returns `Bool`, which avoids `Exception thrown while showing 
  -- test case` message that pops when we use  `=/=`, as it returns `Property`.
  --
  -- also, if the expected failures occur, then quickcheck does not show the 
  -- classifications. yet i have kept them there, because in case the test 
  -- fails, i.e., no errors, then the classifications will help the analysis.
  forAll genBadDecimal $
    \x -> classify (x < 0) "< 0" $
          classify (x == 0) "= 0" $
          classify (x > 0) "> 0" $
          decToBits x /= []
  where genBadDecimal :: Gen Int
        genBadDecimal = (arbitrary :: Gen Int) `suchThat` (>= (-100))
--------------------------------------------------------------------------------
-- | this property is there to show that `binStrToDec` will return incorrect 
-- negative value if the generated "binary" string passed to it has > the 63 
-- bits allowed for `Int` (actually `Int` is 64 bits, but the leftmost bit is a 
-- sign bit, so 63 bits determine `Int` magnitude).  here is a 65-bit example:
-- "00010001100101010110001000011110100001001101101001100011001001001011111" 
-- returns incorrect negative value, because `Int` range is 63 bits.  if you use 
-- `Integer` instead, which has unlimited range, this problem goes away.
--
-- ADVICE: if you are using generated binary string that has no upper limit on 
-- its length, use `Integer` instead of `Int`.
--
-- NOTE: same problem pops up if you use `read x :: Int` to read a long binary 
-- string. again, if you use `read x :: Integer` instead, the problem goes away.
--
-- the conditional check in this property is contrived; i wrote it this way 
-- because i wanted quickcheck to show the "binary" input for the failure case.
--
-- NOTE: this property may occasionaly fail in a particular set of 100 test runs 
-- if the generated string in each of the 100 runs is < 64 in size. but this is 
-- RARE, so if you see a failure, just re-run the test, and you'll see it pass.
prop_binStrToDecNegative :: Property
prop_binStrToDecNegative = expectFailure $ forAll genBinaryStr $
    \bin -> case (binStrToDec (fromBin bin) :: Maybe Int) of
              Nothing  -> False
              Just dec -> if dec >= 0 then bin == bin else bin /= bin
--------------------------------------------------------------------------------
-- | check "binary" string -> decimal conversion.
prop_binStrToDec :: Property
prop_binStrToDec = forAll genBinaryStr $
  \bin -> let xs = fromBin bin in
          classify (bin /= "") "non-empty" $
          classify (allBin bin) "binary" $
          classify (bin /= xs && head xs == '0') "start with 0b0" $
          classify (bin /= xs && head xs == '1') "start with 0b1" $
          -- NOTE: due to laziness, the `let` clause is unevaluated until it is 
          -- needed during expression evaluation.  so in case of an empty 
          -- string, even though `readBin` will error, we see no such errors, 
          -- because for empty string you hit the `Nothing` arm, which does not 
          -- need `readBin`, so `readBin` is never evaluated.
          -- see /u/ andrewc @ https://tinyurl.com/3yv83hf3 (so)
          let exp' :: Integer       = fst . head $ readBin bin
              act  :: Maybe Integer = binStrToDec xs
          in case act of
                Nothing   -> property False
                Just dec  -> dec === exp'
--------------------------------------------------------------------------------
-- | check "bad binary" string -> decimal conversion.
prop_badBinStrToDec :: Property
prop_badBinStrToDec = forAll genBadBinaryStr $
  \bad -> classify (bad == "") "empty" $
          classify (notBin bad) "non-binary" $
          (binStrToDec bad :: Maybe Integer) === Nothing
--------------------------------------------------------------------------------
-- | check equivalence of `binStrToDec` & `binStrToDecS`.
prop_binStrToDecS :: Property
prop_binStrToDecS = forAll genBinaryStr64 $
  \bin -> let xs    :: String     = fromBin bin
              dec1  :: Maybe Int  = binStrToDec xs
              dec2  :: Maybe Int  = binStrToDecS xs
          in classify (length xs < 64) "has size < 64" $
             classify (bin /= "") "non-empty" $
             classify (allBin bin) "binary" $
             classify (bin /= xs && head xs == '0') "start with 0b0" $
             classify (bin /= xs && head xs == '1') "start with 0b1" $
             (dec2 =/= Nothing) .&&.(dec1 === dec2)
--------------------------------------------------------------------------------
-- | check equivalence of `binStrToDec` & `binStrToDecS` for non-binary string.
prop_badBinStrToDecS :: Property
prop_badBinStrToDecS = forAll genBadBinaryStr64 $
  \bad -> classify (length bad < 64) "has size < 64" $
          classify (bad == "") "empty" $
          classify (notBin bad) "non-binary" $
          let dec1 :: Maybe Int = binStrToDec bad
              dec2 :: Maybe Int = binStrToDecS bad
          in (dec2 === Nothing) .&&. (dec1 === dec2)
--------------------------------------------------------------------------------
-- | check "binary" decimal -> decimal conversion.
prop_binDecToDec :: Property
prop_binDecToDec = forAll genBinDec $
  \(bin :: Binary) -> classify (binNum  bin) "binary" $
                      let dec2 :: Int = fst . head . readBin $ show bin
                          num  :: Int = asBinDec bin
                      in case (binDecToDec num :: Maybe Int) of
                          Just dec1 -> dec1 === dec2
                          Nothing   -> property False
--------------------------------------------------------------------------------
-- | check "bad binary" decimal -> decimal conversion.
prop_badBinDecToDec :: Property
prop_badBinDecToDec = forAll genBadBinDec $
  \(bad :: Int) -> classify (decimal bad) "non-binary" $
                   (binDecToDec bad :: Maybe Int) === Nothing
--------------------------------------------------------------------------------
-- | check equivalence of `binDecToDec` & `binDecToDecS` for binary decimal.
prop_binDecToDecS :: Property
prop_binDecToDecS = forAll genBinDec $
  \(bin :: Binary) -> classify (binNum bin) "binary" $
                      let num  :: Int       = asBinDec bin
                          dec1 :: Maybe Int = binDecToDec num
                          dec2 :: Maybe Int = binDecToDecS num
                      in (dec2 =/= Nothing) .&&. (dec1 === dec2)
--------------------------------------------------------------------------------
-- | check equivalence of `binDecToDec` & `binDecToDecS` for non-binary decimal.
prop_badBinDecToDecS :: Property
prop_badBinDecToDecS = forAll genBadBinDec $
  \(bad :: Int) -> classify (decimal bad) "non-binary" $
                   let dec1 :: Maybe Int = binDecToDec bad
                       dec2 :: Maybe Int = binDecToDecS bad
                   in (dec2 === Nothing) .&&. (dec1 === dec2)
--------------------------------------------------------------------------------
-- | check `Word16` -> `Word8` conversion.
-- REF: merge 2 bytes to 1: /u/ hdiederik @ https://tinyurl.com/2p88d8ex (so)
-- REF: bitwise operations: https://en.wikipedia.org/wiki/Bitwise_operation
-- REF: logical shifts: https://www.interviewcake.com/concept/java/bit-shift
-- fromIntegral :: (Integral a, Num b) => a -> b
-- shiftL :: Data.Bits.Bits a => a -> Int -> a
-- (.|.) :: Data.Bits.Bits a => a -> a -> a
prop_encodeWord16 :: Property
prop_encodeWord16 = forAll (arbitrary :: Gen Word16) $
  \w16 -> let w8s :: [Word8] = encodeWord16 w16
          in case w8s of
              (low:high:[]) -> check w16 low high
              _             -> property False
  where check :: Word16 -> Word8 -> Word8 -> Property
        check w16 low high =
            let w16Low  :: Word16 = fromIntegral low
                w16High :: Word16 = shiftL (fromIntegral high :: Word16) 8
            in w16 === (w16High .|. w16Low)
--------------------------------------------------------------------------------
-- | check ASCII `Char` -> `Word8` conversion.
-- isAscii :: Char -> Bool
-- fromIntegral :: (Integral a, Num b) => a -> b
-- Data.Char.chr :: Int -> Char
prop_asciiCharToWord8 :: Property
prop_asciiCharToWord8 = forAll genAsciiChar $
  \x -> classify (isAscii x) "ASCII" $
        case asciiCharToWord8 x of
            Just w8 -> x === (chr . fromIntegral $ w8)
            Nothing -> property False
  where genAsciiChar :: Gen Char
        genAsciiChar = (arbitrary :: Gen Char) `suchThat` isAscii
--------------------------------------------------------------------------------
-- | check non-ASCII `Char` -> `Word8` conversion.
-- isAscii :: Char -> Bool
prop_nonAsciiCharToWord8 :: Property
prop_nonAsciiCharToWord8 = forAll genNonAsciiChar $
  \x -> classify (not . isAscii $ x) "non-ASCII" $
        case asciiCharToWord8 x of
            Nothing -> property True
            _       -> property False
  where genNonAsciiChar :: Gen Char
        genNonAsciiChar = (arbitrary :: Gen Char) `suchThat` (not . isAscii)
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
