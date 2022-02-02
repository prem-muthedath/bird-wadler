{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BinaryLiterals #-}
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
-- Numeric (base 4.14) @ https://tinyurl.com/3kvs3fha
-- Data.Char @ https://tinyurl.com/2c72x8ya
-- Data.Word @ https://tinyurl.com/2p8zph45
-- Data.Bits @ https://tinyurl.com/3b2bu6dt
import Test.QuickCheck
import Text.Read (readMaybe)
import Numeric (readInt, showIntAtBase)
import Data.Char (isDigit, digitToInt, intToDigit, isAscii, chr)
import Data.Word (Word8, Word16, Word64)
import Data.Bits ((.|.), shiftL)

import C2Binary
import Common (ghciRunner)
--------------------------------------------------------------------------------
-- | quickcheck testing -- binary stuff.
--------------------------------------------------------------------------------
-- | common functions.
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

-- | reads a "binary" string, returning the number it represents in decimal.
-- empty string & non-binary string throw error. non-binary string includes 
-- anything that begins with a '-'; i.e., negative numbers are NOT allowed.
readBinary :: Num a => ReadS a
-- readInt :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readBinary bin | good      = readInt 2 f g bin
               | otherwise = error $ "C2BinaryTest.readBinary => "
                                     <> "bad binary: " <> bin
  where good :: Bool            = allBin bin
        f    :: (Char -> Bool)  = isBin
        g    :: (Char -> Int)   = Data.Char.digitToInt

-- | `True` if entire string has only binary characters. returns `False` for "".
allBin :: String -> Bool
allBin [] = False
allBin s  = all isBin s

-- | `True` if character is '0' or '1'.
isBin :: Char -> Bool
isBin = (`elem` ['0', '1'])

-- | `True` if the number, an instance of `Num`, is "binary".
binNum :: (Num a, Show a) => a -> Bool
binNum = allBin . show

-- | `True` if there are any string characters that are neither '0' nor '1'.
notBin :: String -> Bool
notBin "" = True
notBin s  = any nonBin s

-- | `True` if character is non-binary".
nonBin :: Char -> Bool
nonBin = not . isBin

-- | `True` if number is non-binary, i.e., a decimal.
decimal :: (Num a, Show a) => a -> Bool
decimal = notBin . show
--------------------------------------------------------------------------------
-- | generators.
--------------------------------------------------------------------------------
-- | generate `Int` string, with values in range 0 .. 9223372036854775807.
-- NOTE: as documentation, the 1st version (very crude!) listed below.
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
genIntStr :: Gen String
genIntStr = do
  num1 :: Int <- elements [0, upperInt]
  num2 :: Int <- (arbitrary :: Gen Int)
                 `suchThat`
                 (\x -> x > 0 && x < upperInt)
  frequency [ (1, return $ show num1)
            , (20, return $ show num2)
            ]
--------------------------------------------------------------------------------
-- | generate a non-digit character. non-digit means not any in '0' .. '9'.
genNonDigit :: Gen Char
genNonDigit = (arbitrary :: Gen Char) `suchThat` (not . isDigit)
--------------------------------------------------------------------------------
-- | generate "bad" `Int` string.
genBadIntStr :: Gen String
genBadIntStr = do
  x1 :: String <- listOf genNonDigit
  x2 :: String <- show <$> ((arbitrary :: Gen Int)
                            `suchThat`
                            (< 0))
  x3 :: String <- show <$>
          chooseInteger ( (fromIntegral upperInt :: Integer) + 1
                        , (fromIntegral upperInt :: Integer) + 90
                        )
  frequency [ (4, return x1)
            , (2, return x2)
            , (1, return x3)
            ]
--------------------------------------------------------------------------------
-- | generate a non-number.
genNonNum :: Gen String
genNonNum = do
  x1 :: String <- listOf1 (elements "-0")
                  `suchThat`
                  (\xs -> length xs > 1 && length xs <= 64)
  x2 :: String <- listOf ((arbitrary :: Gen Char) `suchThat` isDigit)
                  `suchThat`
                  (\xs -> length xs <= 64)
  x3 :: String <- listOf genNonDigit
                  `suchThat`
                  (\xs -> length xs <= 64)
  frequency [ (1, return $ x1 ++ x2)
            , (2, return x3)
            ]
--------------------------------------------------------------------------------
-- | generate "binary" string.
genBinaryStr :: Gen String
genBinaryStr = listOf1 $ elements ['0', '1']
--------------------------------------------------------------------------------
-- | generate binary string <= 64 in length (i.e., within `Int` range).
genBinaryStr64 :: Gen String
genBinaryStr64 = listOf1 (elements ['0', '1'])
                 `suchThat`
                 (\bin -> length bin <= 64)
--------------------------------------------------------------------------------
-- | generate "bad binary" string.
genBadBinaryStr :: Gen String
genBadBinaryStr = listOf $ (arbitrary :: Gen Char) `suchThat` nonBin
--------------------------------------------------------------------------------
-- | generate "bad binary" string with length <= 64 (within `Int` range).
genBadBinaryStr64 :: Gen String
genBadBinaryStr64 = listOf ((arbitrary :: Gen Char)
                            `suchThat` nonBin)
                    `suchThat` (\bin -> length bin <= 64)
--------------------------------------------------------------------------------
-- | generate "binary" decimal.
genBinDec :: Gen Int
genBinDec = do
  x :: Int <- (arbitrary :: Gen Int) `suchThat` (>= 0)
  -- `showIntAtBase` shows a non-negative `Integral` instance in binary.
  -- showIntAtBase :: (Integral a, Show a) => a -> (Int -> Char) -> a -> ShowS
  let bin :: String = showIntAtBase 2 intToDigit x ""
  return $ read bin
--------------------------------------------------------------------------------
-- | generate "bad binary" decimal.
genBadBinDec :: Gen Int
genBadBinDec = (arbitrary :: Gen Int) `suchThat` decimal
------------------------------------------`--------------------------------------
-- | test the generators!
--------------------------------------------------------------------------------
-- | check if generated `Int` string is valid.
prop_validIntStr :: Property
prop_validIntStr = forAll genIntStr $
  \x -> classify (isNum x) "number" $
        classify (asInt x == 0) "= 0" $
        classify (asInt x > 0 && asInt x < upperInt) "in 1 .. 2 ^ 63 - 2" $
        classify (asInt x == upperInt) "= 2 ^ 63 - 1" $
        (x =/= "") .&&. (asInt x >= 0 .&&. asInt x <= upperInt)
--------------------------------------------------------------------------------
-- | check if generated "bad" `Int` string is indeed bad.
prop_validBadIntStr :: Property
prop_validBadIntStr = forAll genBadIntStr $
  \x -> classify (x == "") "empty" $
        classify (isNum x) "number" $
        classify (isNum x && asInteger x < 0) "< 0" $
        classify (isNum x &&
          asInteger x > (fromIntegral upperInt :: Integer)) "> 2 ^ 63 - 1" $
        classify (notNum x) "non-number" $
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
-- non-numbers. had this not been the case, we will need to use `>=`.
prop_validNonNum :: Property
prop_validNonNum = forAll genNonNum $
  \s  -> classify (s == []) "empty" $
         classify (startsWith00 s) "start with 00" $
         classify (startsWithMinus s) "start with -" $
         classify (startsWithMinus0 s) "start with -0" $
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
prop_validBinaryStr :: Property
prop_validBinaryStr = forAll genBinaryStr $
  \xs -> classify (allBin xs) "binary" $
         classify (xs /= "") "non-empty" $
         classify (xs /= [] && head xs == '0') "starts with 0" $
         classify (xs /= [] && head xs == '1') "starts with 1" $
         classify (length xs > 64) "size > 64" $
         allBin xs
--------------------------------------------------------------------------------
-- | check if generated "binary" string `64` is valid.
prop_validBinaryStr64 :: Property
prop_validBinaryStr64 = forAll genBinaryStr64 $
  \xs -> classify (allBin xs) "binary" $
         classify (xs /= "") "non-empty" $
         classify (xs /= [] && head xs == '0') "starts with 0" $
         classify (xs /= [] && head xs == '1') "starts with 1" $
         classify (length xs <= 64) "size <= 64" $
         length xs <= 64 .&&. allBin xs
--------------------------------------------------------------------------------
-- | check if generated "bad binary" is indeed non-binary.
prop_validBadBinaryStr :: Property
prop_validBadBinaryStr = forAll genBadBinaryStr $
  \xs -> classify (notBin xs) "non-binary" $
         classify (xs == "") "empty" $
         classify (length xs > 64) "size > 64" $
         notBin xs
--------------------------------------------------------------------------------
-- | check if generated "bad binary" `64` is indeed non-binary.
prop_validBadBinaryStr64 :: Property
prop_validBadBinaryStr64 = forAll genBadBinaryStr64 $
  \xs -> classify (notBin xs) "non-binary" $
         classify (xs == "") "empty" $
         classify (length xs <= 64) "size <= 64" $
         length xs <= 64 .&&. notBin xs
--------------------------------------------------------------------------------
-- | check if generated "binary" decimal is valid: >= 0 and has only 1s & 0s.
prop_validBinDec :: Property
prop_validBinDec = forAll genBinDec $
  \bin -> classify (binNum bin) "binary" $
          classify (bin == 0) "= 0" $
          classify (bin > 0) "> 0" $
          binNum bin .&&. property (bin >= 0)
--------------------------------------------------------------------------------
-- | check if generated "bad" binary decimal is indeed non-binary.
prop_validBadBinDec :: Property
prop_validBadBinDec = forAll genBadBinDec $
  \bad -> classify (bad < 0) "< 0" $
          classify (decimal bad) "non-binary" $
          decimal bad
--------------------------------------------------------------------------------
-- | properties.
--------------------------------------------------------------------------------
-- | check `Int` string -> "binary" string conversion.
prop_intStrToBinStr :: Property
prop_intStrToBinStr = forAll genIntStr $
  \x -> classify (isNum x) "number" $
        classify (x == "0") "= 0" $
        classify (x == show upperInt) "= 2^63 - 1" $
        case intStrToBinStr x of
            Left _    -> property False
            Right bin -> check x bin
  where check :: String -> String -> Property
        check x bin = r1 === r2
          where r1 :: [(Int, String)] = readsPrec 0 x
                r2 :: [(Int, String)] = readBinary bin
--------------------------------------------------------------------------------
-- | check "bad" `Int` string -> "binary" string conversion.
prop_badIntStrToBinStr :: Property
prop_badIntStrToBinStr = forAll genBadIntStr $
  \x -> classify (x == "") "empty" $
        classify (notNum x) "non-number" $
        classify (isNum x) "number" $
        case intStrToBinStr x of
            Left _  -> property True
            Right _ -> property False
--------------------------------------------------------------------------------
-- | check if `isNum` works for both numbers as well as non-numbers.
-- check for numbers.
prop_validIsNum :: Property
prop_validIsNum = forAll (arbitrary :: Gen Int) $
  \num  -> classify (num == 0) "0" $
           classify (num < 0) "< 0" $
           classify (num > 0) "> 0" $
           isNum $ show num

-- check for non-numbers.
prop_notIsNum :: Property
prop_notIsNum = forAll genNonNum $ not . isNum
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
-- negative value if the generated "binary" string passed to it has > the 64 
-- bits allowed for `Int`. here is a 65-bit example:
-- "00010001100101010110001000011110100001001101101001100011001001001011111" 
-- returns incorrect negative value, because `Int` range is 64 bits.  if you use 
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
prop_binStrToDecNegative :: Property
prop_binStrToDecNegative = expectFailure $ forAll genBinaryStr $
    \bin -> case (binStrToDec bin :: Maybe Int) of
              Nothing  -> False
              Just dec -> if dec >= 0 then bin == bin else bin /= bin
--------------------------------------------------------------------------------
-- | check "binary" string -> decimal conversion.
prop_binStrToDec :: Property
prop_binStrToDec = forAll genBinaryStr $
  \bin -> classify (bin /= "") "non-empty" $
          classify (allBin bin) "binary" $
          classify (bin /= [] && head bin == '0') "starts with 0" $
          classify (bin /= [] && head bin == '1') "starts with 1" $
          -- NOTE: due to laziness, the `let` clause is unevaluated until it is 
          -- needed during expression evaluation.  so in case of empty string, 
          -- even though `readBinary` will error, we see no such errors, because 
          -- in the `case` for empty string, you hit the `Nothing` arm, which 
          -- does not need `readBinary`, so `readBinary` is never evaluated.
          -- see /u/ andrewc @ https://tinyurl.com/3yv83hf3 (so)
          let exp' :: Integer       = fst . head $ readBinary bin
              act  :: Maybe Integer = binStrToDec bin
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
  \bin -> classify (length bin <= 64) "has size <= 64" $
          classify (bin /= "") "non-empty" $
          classify (allBin bin) "binary" $
          classify (bin /= [] && head bin == '0') "starts with 0" $
          classify (bin /= [] && head bin == '1') "starts with 1" $
          (binStrToDec bin :: Maybe Int) === (binStrToDecS bin :: Maybe Int)
--------------------------------------------------------------------------------
-- | check equivalence of `binStrToDec` & `binStrToDecS` for non-binary string.
prop_badBinStrToDecS :: Property
prop_badBinStrToDecS = forAll genBadBinaryStr64 $
  \bad -> classify (length bad <= 64) "has size <= 64" $
          classify (bad == "") "empty" $
          classify (notBin bad) "non-binary" $
          let dec1 :: Maybe Int = binStrToDec bad
              dec2 :: Maybe Int = binStrToDecS bad
          in (dec2 === Nothing) .&&. (dec1 === dec2)
--------------------------------------------------------------------------------
-- | check "binary" decimal -> decimal conversion.
prop_binDecToDec :: Property
prop_binDecToDec = forAll genBinDec $
  \bin -> classify (binNum  bin) "binary" $
          let dec2 :: Int  = fst . head . readBinary $ show bin
          in case (binDecToDec bin :: Maybe Int) of
                Nothing   -> property False
                Just dec1 -> dec1 === dec2
--------------------------------------------------------------------------------
-- | check "bad binary" decimal -> decimal conversion.
prop_badBinDecToDec :: Property
prop_badBinDecToDec = forAll genBadBinDec $
  \bad -> classify (decimal bad) "non-binary" $
          (binDecToDec bad :: Maybe Int) === Nothing
--------------------------------------------------------------------------------
-- | check equivalence of `binDecToDec` & `binDecToDecS` for binary decimal.
prop_binDecToDecS :: Property
prop_binDecToDecS = forAll genBinDec $
  \bin -> classify (binNum bin) "binary" $
          let dec1 :: Maybe Int = binDecToDec bin
              dec2 :: Maybe Int = binDecToDecS bin
          in (dec2 =/= Nothing) .&&. (dec1 === dec2)
--------------------------------------------------------------------------------
-- | check equivalence of `binDecToDec` & `binDecToDecS` for non-binary decimal.
prop_badBinDecToDecS :: Property
prop_badBinDecToDecS = forAll genBadBinDec $
  \bad -> classify (decimal bad) "non-binary" $
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
