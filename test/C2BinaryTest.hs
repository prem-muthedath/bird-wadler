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
import Numeric (readInt, showIntAtBase)
import Data.Char (digitToInt, intToDigit)
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
-- NOTE: this code same as `(2 :: Int) ^ (63 :: Int) - 1` = 9223372036854775807.
upperInt :: Int
upperInt = maxBound :: Int

-- | convert string -> `Int`.
asInt :: String -> Int
asInt = read

-- | `True` if string represents a valid number, positive or negative.
isNum :: String -> Bool
isNum s = case s of
            ""          -> False
            ('0':[])    -> True
            ('0':_)     -> False
            ('-':[])    -> False
            ('-':'0':_) -> False
            ('-':xs)    -> f xs
            _           -> f s
  where f :: String -> Bool
        f = all (`elem` "0123456789")

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

-- | checks if entire string has binary characters. returns `False` for "".
allBin :: String -> Bool
allBin [] = False
allBin s  = all isBin s

-- | `True` if there are characters in the string that are neither '0' nor '1'.
noBin :: String -> Bool
noBin = not . allBin

-- | checks if character is '0' or '1'.
isBin :: Char -> Bool
isBin = (`elem` ['0', '1'])

-- | checks if character is non-binary".
nonBin :: Char -> Bool
nonBin = not . isBin
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
-- | generate "bad" `Int` string.
genBadIntStr :: Gen String
genBadIntStr = do
  x1 :: String <- listOf $ (arbitrary :: Gen Char)
                           `suchThat`
                           (not . (`elem` "0123456789"))
  x2 :: String <- show <$> ((arbitrary :: Gen Int)
                            `suchThat`
                            (\x -> x < 0 || x > upperInt))
  frequency [ (1, return x1)
            , (1, return x2)
            ]
--------------------------------------------------------------------------------
-- | generate "binary" string.
genBinaryStr :: Gen String
genBinaryStr = listOf $ elements ['0', '1']
--------------------------------------------------------------------------------
-- | generate binary string <= 64 in length (i.e., within `Int` range).
genBinaryStr64 :: Gen String
genBinaryStr64 = listOf (elements ['0', '1'])
                 `suchThat`
                 (\bin -> length bin <= 64)
--------------------------------------------------------------------------------
-- | generate "bad binary" string.
genBadBinaryStr :: Gen String
genBadBinaryStr = listOf1 $ (arbitrary :: Gen Char) `suchThat` nonBin
--------------------------------------------------------------------------------
-- | generate "bad binary" string with length <= 64 (within `Int` range.
genBadBinaryStr64 :: Gen String
genBadBinaryStr64 = listOf1 ((arbitrary :: Gen Char)
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
genBadBinDec = (arbitrary :: Gen Int) `suchThat` (\x -> noBin $ show x)
------------------------------------------`--------------------------------------
-- | test the generators!
--------------------------------------------------------------------------------
-- | check if generated `Int` string is valid.
prop_validIntStr :: Property
prop_validIntStr = forAll genIntStr $
  \x -> classify (isNum x) "number" $
        (x =/= "") .&&. (asInt x >= 0 .&&. asInt x <= upperInt)
--------------------------------------------------------------------------------
-- | check if generated "bad" `Int` string is indeed bad.
prop_validBadIntStr :: Property
prop_validBadIntStr = forAll genBadIntStr $
  \x -> classify (x == "") "empty" $
        classify (isNum x) "number" $
        classify (notNum x) "non-number" $
        check x
  where check :: String -> Property
        check x | x == ""   = property True
                | isNum x   = asInt x < 0 .||. asInt x > upperInt
                | notNum x  = property True
                | otherwise = property False
--------------------------------------------------------------------------------
-- | check if generated "binary" string is valid.
prop_validBinaryStr :: Property
prop_validBinaryStr = forAll genBinaryStr $
  \xs -> classify (allBin xs) "binary" $
         classify (xs == "") "empty" $
         classify (xs /= [] && head xs == '0') "starts with 0" $
         classify (xs /= [] && head xs == '1') "starts with 1" $
         classify (length xs > 64) "size > 64" $
         xs === "" .||. allBin xs
--------------------------------------------------------------------------------
-- | check if generated "binary" string `64` is valid.
prop_validBinaryStr64 :: Property
prop_validBinaryStr64 = forAll genBinaryStr64 $
  \xs -> classify (allBin xs) "binary" $
         classify (xs == "") "empty" $
         classify (xs /= [] && head xs == '0') "starts with 0" $
         classify (xs /= [] && head xs == '1') "starts with 1" $
         classify (length xs <= 64) "size <= 64" $
         length xs <= 64 .&&. (xs === "" .||. allBin xs)
--------------------------------------------------------------------------------
-- | check if generated "bad binary" is indeed non-binary.
prop_validBadBinaryStr :: Property
prop_validBadBinaryStr = forAll genBadBinaryStr $
  \xs -> classify (noBin xs) "non-binary" $
         classify (length xs > 64) "size > 64" $
         noBin xs
--------------------------------------------------------------------------------
-- | check if generated "bad binary" `64` is indeed non-binary.
prop_validBadBinaryStr64 :: Property
prop_validBadBinaryStr64 = forAll genBadBinaryStr64 $
  \xs -> classify (noBin xs) "non-binary" $
         classify (length xs <= 64) "size <= 64" $
         noBin xs
--------------------------------------------------------------------------------
-- | check if generated "binary" decimal is valid: >= 0 and has only 1s & 0s.
prop_validBinDec :: Property
prop_validBinDec = forAll genBinDec $
  \bin -> classify (allBin $ show bin) "binary" $
          classify (bin == 0) "= 0" $
          classify (bin > 0) "> 0" $
          let bins = show bin
          in allBin bins .&&. property (bin >= 0)
--------------------------------------------------------------------------------
-- | check if generated "bad" binary decimal is indeed non-binary.
prop_validBadBinDec :: Property
prop_validBadBinDec = forAll genBadBinDec $
  \bad -> classify (bad < 0) "< 0" $
          classify (noBin $ show bad) "non-binary" $
          noBin $ show bad
--------------------------------------------------------------------------------
-- | properties.
--------------------------------------------------------------------------------
-- | check `Int` string -> "binary` string conversion.
prop_intStrToBinStr :: Property
prop_intStrToBinStr = forAll genIntStr $
  \x -> classify (isNum x) "number" $
        classify (x == "0") "0" $
        classify (x == show upperInt) "2^63 - 1" $
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
-- | check `Word64` -> `[Word64]` binary, "decimal" -> "bits" conversions.
prop_toBinary :: Property
prop_toBinary = forAll genWord64 $
  \x -> classify (x == 0) "zero" $
        toBinary x === decToBits x
  where genWord64 :: Gen Word64
        genWord64 = (arbitrary :: Gen Word64) `suchThat` (>= 0)
--------------------------------------------------------------------------------
-- | check `expectFailure` in "decimal" -> "bits" conversion.
prop_decToBitsFailure :: Property
prop_decToBitsFailure = expectFailure $
  -- NOTE: `/=` returns `Bool`, which avoids `Exception thrown while showing 
  -- test case` message that pops when we use  `=/=`, as it returns `Property`.
  forAll genBadDecimal $ \x -> decToBits x /= []
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
prop_binStrToDecNegative :: Property
prop_binStrToDecNegative = expectFailure $ forAll genBinaryStr $
    \bin -> case (binStrToDec bin :: Maybe Int) of
              Nothing  -> bin == ""
              Just dec -> if dec >= 0 then bin == bin else bin /= bin
--------------------------------------------------------------------------------
-- | check "binary" string -> decimal conversion.
prop_binStrToDec :: Property
prop_binStrToDec = forAll genBinaryStr $
  \bin -> classify (bin == "") "empty" $
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
                Nothing   -> bin === ""
                Just dec  -> dec === exp'
--------------------------------------------------------------------------------
-- | check "bad binary" string -> decimal conversion.
prop_badBinStrToDec :: Property
prop_badBinStrToDec = forAll genBadBinaryStr $
  \bad -> classify (bad /= "") "non-empty" $
          (binStrToDec bad :: Maybe Integer) === Nothing
--------------------------------------------------------------------------------
-- | check equivalence of `binStrToDec` & `binStrToDecS`.
prop_binStrToDecS :: Property
prop_binStrToDecS = forAll genBinaryStr64 $
  \bin -> classify (bin == "") "empty" $
          classify (bin /= [] && head bin == '0') "starts with 0" $
          classify (bin /= [] && head bin == '1') "starts with 1" $
          (binStrToDec bin :: Maybe Int) === (binStrToDecS bin :: Maybe Int)
--------------------------------------------------------------------------------
-- | check equivalence of `binStrToDec` & `binStrToDecS` for non-binary string.
prop_badBinStrToDecS :: Property
prop_badBinStrToDecS = forAll genBadBinaryStr64 $
  \bad -> let dec1 :: Maybe Int = binStrToDec bad
              dec2 :: Maybe Int = binStrToDecS bad
          in (dec2 === Nothing) .&&. (dec1 === dec2)
--------------------------------------------------------------------------------
-- | check "binary" decimal -> decimal conversion.
prop_binDecToDec :: Property
prop_binDecToDec = forAll genBinDec $
  \bin -> classify (allBin $ show bin) "binary" $
          let dec2 :: Int  = fst . head . readBinary $ show bin
          in case (binDecToDec bin :: Maybe Int) of
                Nothing   -> property False
                Just dec1 -> dec1 === dec2
--------------------------------------------------------------------------------
-- | check "bad binary" decimal -> decimal conversion.
prop_badBinDecToDec :: Property
prop_badBinDecToDec = forAll genBadBinDec $
  \bad -> classify (noBin $ show bad) "non-binary" $
          (binDecToDec bad :: Maybe Int) === Nothing
--------------------------------------------------------------------------------
-- | check equivalence of `binDecToDec` & `binDecToDecS` for binary decimal.
prop_binDecToDecS :: Property
prop_binDecToDecS = forAll genBinDec $
  \bin -> classify (allBin $ show bin) "binary" $
          (binDecToDec bin :: Maybe Int) === (binDecToDecS bin :: Maybe Int)
--------------------------------------------------------------------------------
-- | check equivalence of `binDecToDec` & `binDecToDecS` for non-binary decimal.
prop_badBinDecToDecS :: Property
prop_badBinDecToDecS = forAll genBadBinDec $
  \bad -> classify (noBin $ show bad) "non-binary" $
          let dec1 :: Maybe Int = binDecToDec bad
              dec2 :: Maybe Int = binDecToDecS bad
          in (dec2 === Nothing) .&&. (dec1 === dec2)
--------------------------------------------------------------------------------
-- | check `Word16` -> `Word8` conversion.
-- REF: merge 2 bytes to 1: /u/ hdiederik @ https://tinyurl.com/2p88d8ex (so)
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
