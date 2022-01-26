{-# LANGUAGE ScopedTypeVariables #-}

-- | chapter 2: bird & wadler, introduction to functional programming.
-- "binary stuff: includes `Data.Word`, `Data.Bits`, to-and-fro conversions.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler` to start GHCi.
--  3. next, at GHCi prompt, enter `import C2Binary`.
--  4. you can then invoke any of the top-elevel functions.
-- Prem Muthedath, 26 JAN 2022.

--------------------------------------------------------------------------------
module C2Binary where
--------------------------------------------------------------------------------
-- Data.Char @ https://tinyurl.com/2c72x8ya
-- Text.Read @ https://tinyurl.com/36etaccn
-- Data.Word @ https://tinyurl.com/2p8zph45
-- Data.Bits @ https://tinyurl.com/3b2bu6dt
-- Control.Monad @ https://tinyurl.com/mtvj95yx
import Data.Char (intToDigit, ord)
import Text.Read (readMaybe)
import Data.Word (Word8, Word16, Word32)
import Data.Bits (FiniteBits, finiteBitSize, testBit, shift, shiftR, (.&.))
import Control.Monad (foldM)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | `Int` string -> Binary string.
--------------------------------------------------------------------------------
-- | convert `Int` string to 32-bit binary string.
-- NOTE: `Int` value in string must be in [0, 2^32 -1] or (0, 4294967295).
-- `Int` string example: "255".
-- "binary" string contains only '1' and '0'; 8-bit example: "11001001".
-- REF: /u/ pat, /u/ delta @ https://tinyurl.com/mpers6md (so)
intStrToBinStr :: String -> Either String String
intStrToBinStr xs = do
    num :: Int <- toInt
    if num >= 0 && num <= upper
       then Right $ toBinStr $ intToWord32 num
       else Left msg
  where toInt :: Either String Int
        -- readMaybe :: Read a => String -> Maybe a
        toInt = case (readMaybe xs :: Maybe Int) of
                    Just n  -> Right n
                    Nothing -> Left $ xs <> " is not an Int"
        upper :: Int
        upper = 2 ^ (32 :: Int) - 1
        toBinStr :: Word32 -> String
        -- intToDigit :: Int -> Char
        toBinStr = map (intToDigit . word32ToInt) . toBinary
        msg :: String
        msg = "out-of-range: " <> xs <> " not in [0, " <> show upper <> "]."

-- | generate 32-bit binary representation of a `Word32` as a `[Word32]`.
-- REF: /u/ pat, /u/ delta @ https://tinyurl.com/mpers6md (so)
-- decimal-to-binary conversion: https://tinyurl.com/ycktc6va (wikihow.com)
toBinary :: Word32 -> [Word32]
toBinary = go 32 []
  where go :: Int -> [Word32] -> Word32 -> [Word32]
        -- NOTE:
        --  1. we continue the recursion until `n == 0`, because we want the 
        --     leading zeroes in the binary representation, which means you 
        --     can't stop the coversion when the value becomes 0.
        --  2. `go` prepends to a list.
        --  3. /u/ pat's code faulty because it reverses the list when `n = 0`.
        go 0 acc _ = acc
        go n acc x = go (n - 1) (bit : acc) x'
          -- type annotation in `where` requires `ScopedTypeVariables` extn.
          where (x', bit) :: (Word32, Word32) = x `divMod` 2
--------------------------------------------------------------------------------
-- | `Int` -> `Word32`, `Word32` -> `Int`
--------------------------------------------------------------------------------
-- | convert `Int` to `Word32`
-- fromIntegral :: (Integral a, Num b) => a -> b
-- `Word32` has a `Num` instance.
-- REF: /u/ hao @ https://tinyurl.com/fv963jf8 (so)
intToWord32 :: Int -> Word32
intToWord32 = fromIntegral

-- | convert `Word32` to `Int`.
-- fromIntegral :: (Integral a, Num b) => a -> b
-- `Word32` has an `Integral` instance.
-- REF: /u/ astiefel @ https://tinyurl.com/s5zwyta3 (so)
word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral
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
            | otherwise = error $ show x <> " is not >= 0."
  where toBitList :: [Bool]
        toBitList = reverse [testBit x i | i <- [0 .. (finiteBitSize x - 1)]]
--------------------------------------------------------------------------------
-- | `Word16` -> `[Word8]'
--------------------------------------------------------------------------------
-- | convert `Word16` to a list of `Word8`; list has just 2 elements, obviously.
-- REF: code from /u/ lee duhem @ https://tinyurl.com/2p84ew7d (so)
-- REF: `0xFF` & `0xFF00`: /u/ rayryeng @ https://tinyurl.com/2p9but4r (so)
-- REF: bitwise operations: https://en.wikipedia.org/wiki/Bitwise_operation
-- REF: logical shifts: https://www.interviewcake.com/concept/java/bit-shift
-- NOTE: in code below, you need either `0xFF00` mask or `shiftR` but not both, 
-- because one makes the other redundant: see /u/ rayryeng in `REF` above.
encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [ x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8 ]
--------------------------------------------------------------------------------
-- | `Char` -> `Word8`.
--------------------------------------------------------------------------------
-- convert a `Char` to `Word8`.
-- REF: /u/ Jonathan Prieto-Cubides @ https://tinyurl.com/3jk27h44 (so)
charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord
--------------------------------------------------------------------------------
