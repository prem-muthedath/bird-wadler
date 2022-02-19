{-# LANGUAGE ScopedTypeVariables #-}

-- | chapter 2: bird & wadler, introduction to functional programming.
-- "binary stuff: includes `Data.Word`, `Data.Bits`, to-and-fro conversions.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler` to start GHCi.
--  3. next, at GHCi prompt, enter `import C2Binary`.
--  4. you can then invoke any of the top-elevel functions.
--  5. OR instead of steps 2-4,
--      a) run `cabal v2-repl :bird-wadler-test` to start GHCi;
--      b) at GHCi prompt, enter `import C2BinaryTest`;
--      c) invoke `C2BinaryTest.ghciQC` to run all quickcheck tests.
-- author: Prem Muthedath, 26 JAN 2022.

--------------------------------------------------------------------------------
module C2Binary where
--------------------------------------------------------------------------------
-- Data.Char @ https://tinyurl.com/2c72x8ya
-- Text.Read @ https://tinyurl.com/36etaccn
-- Data.Word @ https://tinyurl.com/2p8zph45
-- Data.Bits @ https://tinyurl.com/3b2bu6dt
-- Control.Monad @ https://tinyurl.com/mtvj95yx
import Data.Char (intToDigit, ord, isAscii)
import Text.Read (readMaybe)
import Data.Word (Word8, Word16, Word64)
import Data.Bits (FiniteBits, finiteBitSize, testBit, shift, shiftR, (.&.))
import Control.Monad (foldM)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | `Int` string -> Binary string.
--------------------------------------------------------------------------------
-- | convert `Int` string to 64-bit binary string.
-- REF: /u/ pat, /u/ delta @ https://tinyurl.com/mpers6md (so)
-- NOTES:
--  1. allowed `Int` range:  0 .. 2^63 -1 or 0 .. 9223372036854775807.
--  2. `Int` string example: "255".
--  3. "binary" string contains only '1' and '0'; 8-bit example: "11001001".
--  4. in this code, stuff like "9\f", as well as "\f9", end up as a valid 
--     number because `readmaybe "9\f"` = 9. in fact, any number string starting 
--     or ending with any character determined by `Data.Char.isSpace` as space 
--     will be read as a number. for example, `readMaybe "\r9\f\r\t" = 9`.
--  SAMPLE Int OUT-OF-RANGE INPUT/OUTPUT:
--    intStrToBinStr "9797716803182633900815968305162\t\r\n\t" =
--    Left "out-of-range: 9797716803182633900815968305162\t\r\n\t not in 0 .. 9223372036854775807."
intStrToBinStr :: String -> Either String String
intStrToBinStr xs = do
    num :: Integer <- toInt
    if num >= 0 && num <= (fromIntegral upper :: Integer)
       then Right $ toBinStr $ intToWord64 (fromInteger num :: Int)
       else Left msg
  where toInt :: Either String Integer
        -- readMaybe :: Read a => String -> Maybe a
        toInt = case (readMaybe xs :: Maybe Integer) of
                    Just n  -> Right n
                    Nothing -> Left $ xs <> " is not an Integer"
        upper :: Int
        -- maxBound :: Bounded a => a
        upper = maxBound :: Int   -- same as (2 :: Int) ^ (63 :: Int) - 1
        toBinStr :: Word64 -> String
        -- intToDigit :: Int -> Char; `Int` should be in range 0 .. 15.
        toBinStr = map (intToDigit . word64ToInt) . toBinary
        msg :: String
        msg = "out-of-range: " <> xs <> " not in 0 .. " <> show upper <> "."

-- | generate 64-bit binary representation of a `Word64` as a `[Word64]`.
-- REF: /u/ pat, /u/ delta @ https://tinyurl.com/mpers6md (so)
-- decimal-to-binary conversion: https://tinyurl.com/ycktc6va (wikihow.com)
toBinary :: Word64 -> [Word64]
toBinary = go 64 []
  where go :: Int -> [Word64] -> Word64 -> [Word64]
        -- NOTE:
        --  1. we continue the recursion until `n == 0`, because we want the 
        --     leading zeroes in the binary representation, which means you 
        --     can't stop the coversion when the value becomes 0.
        --  2. `go` prepends to a list.
        --  3. /u/ pat's code faulty because it reverses the list when `n = 0`.
        go 0 acc _ = acc
        go n acc x = go (n - 1) (bit : acc) x'
          -- type annotation in `where` requires `ScopedTypeVariables` extn.
          -- divMod :: Integral a => a -> a -> (a, a)
          where (x', bit) :: (Word64, Word64) = x `divMod` 2
--------------------------------------------------------------------------------
-- | `Int` -> `Word64`, `Word64` -> `Int`
--------------------------------------------------------------------------------
-- | convert `Int` to `Word64`
-- fromIntegral :: (Integral a, Num b) => a -> b
-- `Word64` has a `Num` instance.
-- REF: /u/ hao @ https://tinyurl.com/fv963jf8 (so)
intToWord64 :: Int -> Word64
intToWord64 = fromIntegral

-- | convert `Word64` to `Int`.
-- fromIntegral :: (Integral a, Num b) => a -> b
-- `Word64` has an `Integral` instance.
-- REF: /u/ astiefel @ https://tinyurl.com/s5zwyta3 (so)
word64ToInt :: Word64 -> Int
word64ToInt = fromIntegral
--------------------------------------------------------------------------------
-- | "Binary" string -> decimal.
--------------------------------------------------------------------------------
-- | convert string representing binary to decimal.
-- NOTE: "binary" string considers only values >= 0.
-- "binary" string contains only '1' and '0'; example: "11001001".
-- "decimal"'s type is an `Integral` instance.
-- REF: see /u/ iceman + several examples @ https://tinyurl.com/2p89255z (so)
-- author: Prem Muthedath.
binStrToDec :: forall a. (Integral a) => String -> Maybe a
binStrToDec [] = Nothing
binStrToDec xs = go 0 xs
  where go :: a -> String -> Maybe a
        go acc [] = pure acc
        go acc a@(y:ys) | y == '1'  = go ((2 ^ (length a - 1)) + acc) ys
                        | y == '0'  = go acc ys
                        | otherwise = Nothing

-- | convert string representing binary to decimal using `Data.Bits.shift`.
-- "decimal"'s type is an instance of both `Integral` & `FiniteBits`.
-- REF: /u/ steerio @ https://tinyurl.com/2p89255z (so)
-- REF: bitwise operations: https://en.wikipedia.org/wiki/Bitwise_operation
-- REF: logical shifts: https://www.interviewcake.com/concept/java/bit-shift
-- NOTE: + condition for `""`, which is not there in /u/ steerio's code.
-- foldM ~ foldl: it starts at the leftmost or 1st element of the list.
-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
-- shift :: Bits a => a -> Int -> a
binStrToDecS :: forall a. (Integral a, FiniteBits a) => String -> Maybe a
binStrToDecS "" = Nothing
binStrToDecS xs = foldM step 0 xs
  where step :: a -> Char -> Maybe a
        step acc '0' = pure $ shift acc 1
        step acc '1' = pure $ shift acc 1 + 1
        step _ _     = Nothing
--------------------------------------------------------------------------------
-- | "Binary" decimal (>= 0) -> decimal.
--------------------------------------------------------------------------------
-- | convert a "binary" decimal to decimal.
-- "binary" decimal has digits 1 or 0.  for example, `11011101 :: Int`.
-- "binary" decimal's type is an `Integral` instance..
-- output "decimal" has the same type as "binary" decimal.
-- REF: code from /u/ willem van onsem @ https://tinyurl.com/32bv54jd (so)
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- div :: Integral a => a -> a -> a
-- mod :: Integral a => a -> a -> a
binDecToDec :: forall a. (Integral a) => a -> Maybe a
binDecToDec 0 = Just 0
binDecToDec i | last' `elem` [0, 1]
                = fmap (\x -> 2 * x + last') (binDecToDec (div i 10))
              | otherwise = Nothing
              where last' :: a = i `mod` 10

-- | same as `binIntToDec` but uses a different algorithm involving bit shifts.
-- author: Prem Muthedath.
binDecToDecS :: forall a. (FiniteBits a, Integral a, Show a) => a -> Maybe a
binDecToDecS = binStrToDecS . show
--------------------------------------------------------------------------------
-- | +ve Decimal -> [bit].
--------------------------------------------------------------------------------
-- | convert `decimal -- Int`, `Word` -- to `[bit]`; numbers >=0 considered.
-- "decimal"'s type is an instance of both `Integral` & `FiniteBits`; examples: 
-- `Int`, `Word`, etc., that are instances of both `Integral` and `FiniteBits`.
-- `[bit]` has  elements 1 or 0, and they have the same type as decimal.
-- REF: /u/ cmcdragonkai, /u/ gspr @ https://tinyurl.com/yfte299y (so)
-- REF: /u/ dfeuer, /u/ thomas m. debuisson @ https://tinyurl.com/s5zwyta3 (so) 
-- class Bits b => FiniteBits b where
--    finiteBitSize :: b -> Int
--    countLeadingZeros :: b -> Int
--    countTrailingZeros :: b -> Int
--    {-# MINIMAL finiteBitSize #-}
--        Defined in ‘Data.Bits’
-- both `Int` & `Word` are instances of both `Bits` & `FiniteBits`.
-- finiteBitSize :: FiniteBits b => b -> Int
-- testBit :: Bits a => a -> Int -> Bool
decToBits :: (FiniteBits a, Integral a, Show a) => a -> [a]
decToBits x | x >= 0 = map (\y -> case y of { True -> 1; False -> 0 }) toBitList
            | otherwise = error $ "C2Binary.decToBits => "
                                  <> "argument " <>  show x <> " is not >= 0."
  where toBitList :: [Bool]
        -- we `reverse` the list because the 0th bit is the least significant 
        -- bit, so should be at the right end. for example, `Data.Bits.testBit 
        -- (254 :: Word8) 0 = False`.
        toBitList = reverse [testBit x i | i <- [0 .. (finiteBitSize x - 1)]]
--------------------------------------------------------------------------------
-- | `Word16` -> `[Word8]'
--------------------------------------------------------------------------------
-- | convert `Word16` to a list of `Word8`; list has just 2 elements, obviously.
-- REF: code from /u/ lee duhem @ https://tinyurl.com/2p84ew7d (so)
-- REF: `0xFF`: /u/ oscar liang @ https://tinyurl.com/5n97nar7
-- REF: `0xFF00`: /u/ rayryeng @ https://tinyurl.com/2p9but4r (so)
-- REF: bitwise operations: https://en.wikipedia.org/wiki/Bitwise_operation
-- REF: logical shifts: https://www.interviewcake.com/concept/java/bit-shift
-- fromIntegral :: (Integral a, Num b) => a -> b
-- (.&.) :: Data.Bits.Bits a => a -> a -> a
-- shiftR :: Data.Bits.Bits a => a -> Int -> a
encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [ x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8]
--------------------------------------------------------------------------------
-- | ASCII `Char` -> `Word8`.
--------------------------------------------------------------------------------
-- convert an ASCII `Char` to `Word8`.
-- returns `Nothing` for non-ASCII characters.
-- REF: /u/ Jonathan Prieto-Cubides @ https://tinyurl.com/3jk27h44 (so)
-- Prem Muthedath greatly modified the code in REF.
-- isAscii :: Char -> Bool
-- fromIntegral :: (Integral a, Num b) => a -> b
-- Data.Char.ord :: Char -> Int
asciiCharToWord8 :: Char -> Maybe Word8
asciiCharToWord8 x | isAscii x = Just $ fromIntegral . ord $ x
                   | otherwise = Nothing
--------------------------------------------------------------------------------
