{-# LANGUAGE ScopedTypeVariables #-}

-- | chapter 2: bird & wadler, introduction to functional programming.
-- `Read` class: example.
-- usage:
--  1. `cd` to `bird-wadler` directory, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler` to start GHCi.
--  3. next, at GHCi prompt, enter `import C2Read`.
--  4. you can then invoke any of the top-elevel functions.
--  5. OR instead of steps 3 & 4, at GHCi prompt, enter `import C2ReadTest`, & 
--     then you can invoke `runAllQC` to run all quickcheck tests.
-- author: Prem Muthedath, NOV 2021.

--------------------------------------------------------------------------------
module C2Read where

-- | Text.Read
-- REF: https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-Read.html

-- | Haskell 2010 report, chapter 9, "Standard Prelude".
-- REF: https://tinyurl.com/e4zdetc

-- | Haskell 1998 report, chapter 9, "Character Utilities".
-- REF: https://www.cse.iitb.ac.in/~as/fpcourse/haskell98_library/char.html

-- | GHC.Read
-- REF: https://tinyurl.com/h4sm4nnm

-- | Gentle Introduction to Haskell, chapter 8, "Standard Haskell Classes".
-- https://tinyurl.com/yhyywjc9

-- | class Read a where
--      readsPrec  :: Int -> ReadS a
--      readList   :: ReadS [a]

-- Minimal complete definition:
--      readsPrec

-- `class Read`: parsing of strings to produce values.
-- derived instances of `Read` make the following assumptions, which derived 
-- instances of `Show` obey:
--    1. if the constructor is defined to be an infix operator, then the derived 
--       instance of `Read` will parse only the infix applications of the 
--       constructor (not the prefix form).
--    2. associativity is not used to reduce the occurrence of parenthesis, 
--       although precedence may be.
--    3. if the constructor is defined using record syntax, the derived `Read` 
--       will parse only the record-syntax form, and furthermore, the fields 
--       must be given in the same order as the original declaration.
--    4. the derived `Read` instance allows arbitrary haskell whitespace between 
--       tokens of the input string; extra parenthesis are also allowed.

-- | `ReadS a` type.
-- it is a parser for a type a, represented as a function that takes a String 
-- and returns a list of possible parses as (a, String) pairs.
-- type ReadS a = String -> [(a, String)]

-- | `readsPrec` function.
--      1. it attempts to parse a value from the front of the string, returning 
--         a list of (parsed value, remaining string) pairs. if there is no 
--         successful parse, the returned list is empty.
--      2. it's first argument (an `Int`) is the operator precdence of the 
--         enclosing context (0-11).
--      3. derived instances of `Read` and `Show` satisfy the following:
--            (x,"") is an element of (readsPrec d (showsPrec d x "")).
--         that is, readsPrec parses the string produced by showsPrec, and 
--         delivers the value that showsPrec started with.
-- readsPrec :: Int -> ReadS a

-- | `reads` function.
-- `reads` is a parser for any instance of `Read`.
-- reads :: Read a => ReadS a
-- reads = readsPrec 0

-- | `read` function.
-- it reads input from a string, which MUST be completely consumed by the input 
-- process; `read` FAILS with an `error` if the parse is unsuccessful.
-- read :: Read a => String -> a
-- read s  =
--     case [x | (x,t) <- reads s, ("","") <- lex t] of
--       [x] -> x
--       []  -> error "PreludeText.read: no parse"
--       _   -> error "PreludeText.read: ambiguous parse"

-- | `readParen` function.
-- `readParen True p` parses what `p` parses but surrounded with parenthesis;
-- `readParen False p` parses what `p` parses but with optional parenthesis; the 
-- "optional parenthesis" part comes from `++` in the below code.
-- readParen :: Bool -> ReadS a -> ReadS a
-- readParen b g  =  if b then mandatory else optional
--    where optional    :: ReadS a
--          optional r  = g r ++ mandatory r
--          mandatory   :: ReadS a
--          mandatory r = [(x,u) |
--                ("(",s) :: (String, String) <- lex r,
--                (x,t)   :: (a, String)      <- optional s,
--                (")",u) :: (String, String) <- lex t ]

-- | `readList` function.
-- readList "[1, 2, 3]" :: [([Int], String)] = [([1,2,3],"")]
-- readList "[Leaf 1, Leaf 2 :^: Leaf 3]" :: [([Tree Int], String)]
--    = [([Leaf 1,Leaf 2 :^: Leaf 3],"")]
-- https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1710009
-- readList :: forall a. (Read a) => ReadS [a]
-- readList = readParen False
--               (\r -> [pr | ("[",s) :: (String, String) <- lex r,
--                            pr      :: ([a], String)    <- readl s])
--     where readl :: ReadS [a]
--           -- lists from `lex ("]")` & (`reads`,`readl'`) mutually exclusive.
--           readl  s = [([],t)   | ("]",t) :: (String, String) <- lex s] ++
--                      [(x:xs,u) | (x,t)  :: (a, String)   <- reads s,
--                                  (xs,u) :: ([a], String) <- readl' t]
--           readl' :: ReadS [a]
--           readl' s = [([],t)   | ("]",t) :: (String, String) <- lex s] ++
--                      [(x:xs,v) | (",",t) :: (String, String) <- lex s,
--                                  (x,u)   :: (a, String)      <- reads t,
--                                  (xs,v)  :: ([a], String)    <- readl' u]

-- | `lex` function.
-- `lex` reads a single lexeme from the input, discarding initial white space, 
-- returning the characters that constitute the lexeme. if the input string 
-- contains only white space, `lex` returns a single successful 'lexeme' 
-- consisting of the empty string -- `lex "" = [("", "")]`. if there is no legal 
-- lexeme at the beginning of the input string, `lex` fails (i.e., returns []).
-- lex :: ReadS String

--------------------------------------------------------------------------------
import Control.Monad (guard)
--------------------------------------------------------------------------------
-- | example 1: `Read` instance for `Tree` that has a custom `Show` instance.
-- source: chapter 11, `specification of derived instances', `section 5`, 
-- haskell 2010 report @ https://tinyurl.com/nxh9d2y

infixr 5 :^:
data Tree a =  Leaf a  |  Tree a :^: Tree a deriving Eq

instance (Show a) => Show (Tree a) where
       showsPrec d (Leaf m) = showParen (d > app_prec) $
            showString "Leaf " . showsPrec (app_prec+1) m
         where app_prec = 10

       showsPrec d (u :^: v) = showParen (d > up_prec) $
            showsPrec (up_prec+1) u .
            showString " :^: "      .
            showsPrec (up_prec+1) v
         where up_prec = 5
         -- NOTE: right associativity of :^: is ignored!

-- | `Read` instance for `Tree`
instance (Read a) => Read (Tree a) where
  -- how does `readsPrec` work?  it seems to be infinitely recursive!
  -- see https://paste.ofcode.org/DCiTkXtTH7YHPbcY97b5Bd
  -- well, using type annotation, as in below code, we can see that `readsPrec` 
  -- is polymorphic, so the top-level `readsPrec` is not infinitely recursive.

  -- readsPrec 0 "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: [(Tree Int, String)]
  --    = [(Leaf 1 :^: Leaf 2 :^: Leaf 3,""),
  --       (Leaf 1," :^: (Leaf 2 :^: Leaf 3)")]
  --
  -- read "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: Tree Int
  --    = Leaf 1 :^: Leaf 2 :^: Leaf 3
  --
  -- let us walk through the code to see how it gets these results.
  --  readsPrec 0 "Leaf 1 :^: (Leaf 2 :^: Leaf 3)"  :: [(Tree Int, String)] = 
  --    1. readParen (0 > 5) g r
  --       +  optional = g r ++ mandatory r
  --       +      g r
  --       +        readsPrec 6 r
  --       +        + readParen (6 > 5) g r
  --       +        +   mandatory r  => lex ("(", ...) parse FAILS
  --       +        + readParen (6 > 10) g' r
  --       +        + + optional = g' r ++ mandatory r
  --       +        + + + g' r
  --       +        + + + +  lex r => ("Leaf", " " 1 :^: ...") => PASS
  --       +        + + + +  readsPrec 11 " 1 :^: ..." => (1, " :^:...") => PASS
  --       +        + + + RETURNS (Leaf 1, " :^:...")
  --       +        + + + mandatory r => FAILS in lex ("(", ...)
  --       +        + + RETURNS (Leaf 1, " :^:...")
  --       +        + RETURNS (Leaf 1, " :^: (Leaf 2 :^: Leaf 3)")
  --       +        RETURNS (Leaf 1, " :^: (Leaf 2 :^: Leaf 3)")
  --       +        (":^:", t) <- lex (" :^:...") => PASS
  --       +        readsPrec 6 " (Leaf 2 :^: Leaf 3)"
  --       +        + readParen (6 > 5) g " (Leaf 2 :^:...)"
  --       +        + + mandatory " (Leaf 2 :^:...)"
  --       +        + + + lex ("(", ...) => PASS
  --       +        + + + optional "Leaf 2 :^: Leaf 3)"
  --       +        + + + + g "Leaf 2 :^: Leaf 3)"
  --       +        + + + + + readsPrec 6 "Leaf 2 ,,,)"
  --       +        + + + + + +  readParen (6 > 5) g "Leaf 2 ... )"
  --       +        + + + + + +    mandatory "Leaf 2 ...)" => FAILS
  --       +        + + + + + +  readParen (6 > 10) g' "Leaf 2 ... )"
  --       +        + + + + + +  + optional = g' r ++ mandatory r
  --       +        + + + + + +  + + g' r
  --       +        + + + + + +  + + + lex "Leaf 2 ... )" => PASS
  --       +        + + + + + +  + + + readsPrec 11 " 2 ... )" => PASS
  --       +        + + + + + +  + + RETURNS (Leaf 2, " :^: Leaf 3)")
  --       +        + + + + + +  + + mandatory r => FAILS in lex ("(" ...)
  --       +        + + + + + +  + RETURNS (Leaf 2, " :^: Leaf 3)")
  --       +        + + + + + +  RETURNS (Leaf 2, " :^: Leaf 3)")
  --       +        + + + + + RETURNS (Leaf 2, " :^: Leaf 3)")
  --       +        + + + + + lex " :^: Leaf 3)" => PASS
  --       +        + + + + + readsPrec 6 " Leaf 3)"
  --       +        + + + + + +  readParen (6 > 5) " Leaf 3)"
  --       +        + + + + + +    mandatory " Leaf 3)" => FAILS in lex ("(", 
  --       +        + + + + + +    ...)
  --       +        + + + + + +  readParen (6 > 10) " Leaf 3)"
  --       +        + + + + + +  + optional = g' r ++ mandatory r
  --       +        + + + + + +  + + g' r
  --       +        + + + + + +  + + + lex " Leaf 3)" => PASS
  --       +        + + + + + +  + + + readsPrec " 3)" => PASS
  --       +        + + + + + +  + + RETURNS (Leaf 3, ")")
  --       +        + + + + + +  + + mandatory " Leaf 3)" => FAILS in lex ("(", 
  --       +        + + + + + +  + + ...)
  --       +        + + + + + +  + RETURNS (Leaf 3, ")")
  --       +        + + + + + +  RETURNS (Leaf 3, ")")
  --       +        + + + + + RETURNS (Leaf 3, ")")
  --       +        + + + + RETURNS (Leaf 2 :^: Leaf 3, ")")
  --       +        + + + + mandatory "Leaf 2 :^: Leaf 3)" => FAILS in lex ("(")
  --       +        + + + RETURNS (Leaf 2 :^: Leaf 3, ")")
  --       +        + + + lex (")","") => PASS
  --       +        + + RETURNS (Leaf 2 :^: Leaf 3, "")
  --       +        + RETURNS (Leaf 2 :^: Leaf 3, "")
  --       +        + readParen (6 > 10) g' " (Leaf 2 :^:...)"
  --       +        + + optional " (Leaf 2 :^: ...)"
  --       +        + + + g' " (leaf 2 :^: ...)"
  --       +        + + +   lex " (Leaf 2 :^: ...)" => FAILS
  --       +        + + + FAILS
  --       +        + + + mandatory " (Leaf 2 :^: ...)"
  --       +        + + +  lex " (Leaf 2 ... )" => PASS
  --       +        + + +  optional "Leaf 2 :^: Leaf 3)"
  --       +        + + +    g' "Leaf 2 :^: Leaf 3)"
  --       +        + + +      lex "Leaf 2 :^: Leaf 3)" => PASS
  --       +        + + +      readsPrec " 2 :^: Leaf 3)" => PASS
  --       +        + + +    RETURNS (Leaf 2, " :^: Leaf 3)")
  --       +        + + +    mandatory "Leaf 2 :^: Leaf 3)" => FAILS in lex
  --       +        + + +  RETURNS (Leaf 2, " :^: Leaf 3)")
  --       +        + + +  lex " :^: Leaf 3)" => FAILS in lex (")", ...)
  --       +        + + + FAILS
  --       +        + + FAILS
  --       +        + FAILS
  --       +        RETURNS (Leaf 2 :^: Leaf 3, "")
  --       RETURNS (Leaf 1 :^: Leaf 2 :^: Leaf 3, "")
  -- ++ 2. readParen (0 > 10) g' r
  --       +  optional = g' r ++ mandatory r
  --       +  + g' r
  --       +  + + lex r => ("Leaf", " 1 :^: ...") => PASS
  --       +  + + readsPrec 11 "1 :^: ..." => (1, ":^:...") => PASS
  --       +  + RETURNS (Leaf 1, " :^:...")
  --       +  + mandatory r => FAILS in lex ("(", ...)
  --       RETURNS (Leaf 1, " :^:...")
  -- = [(Leaf 1 :^: Leaf 2 :^: Leaf 3,""),(Leaf 1, " :^: (Leaf 2 :^: Leaf 3)")]
  --
  -- read "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: Tree Int =
  --    case [ x |
  --        (x, t)    <- readsPrec 0 "Leaf 1 :^: ..." :: [(Tree Int, String)],
  --        ("", "")  <- lex t] of
  --      [x] -> x
  --      []  -> error
  --      _   -> error
  --
  --  `readsPrec` gives  a 2-element list (see above), but only the 1st element 
  --  satisfies the condition `lex t = ("", "")`, so we get
  --   = Leaf 1 :^: Leaf 2 :^: Leaf 3
  ------------------------------------------------------------------------------
  --`(d0 > up_prec)` condition corresponds to the one in `showsPrec`.
  readsPrec d0 r0 = readParen' (d0 > up_prec)
                      (\r -> [(u:^:v, w) |
                             (u, s)     :: (Tree a, String) <- readsPrec (up_prec+1) r,
                             (":^:", t) :: (String, String) <- lex s,
                             (v, w)     :: (Tree a, String) <- readsPrec (up_prec+1) t])
                      r0
  -- `(d0 > app_prec)` condition corresponds to the one in `showsPrec`.
                    ++ readParen' (d0 > app_prec)
                        (\r -> [(Leaf m, t) |
                             ("Leaf", s)  :: (String, String) <- lex r,
                             (m, t)       :: (a, String)      <- readsPrec (app_prec+1) s])
                        r0
                where app_prec = 10
                      up_prec = 5

--------------------------------------------------------------------------------
-- | ALTERNATIVE FORMULATION (AS AN EXPERIMENT).
-- below definition is equivalent to or better than the `readsPrec` above.

-- the `readsPrec` definition above uses `++` operator to tie up things, but 
-- usually, though not always, the two sides of `++` are mutually exclusive: 
-- i.e., if one occurs, the other won't. for evidence of this claim, see the 
-- code walk-through given above.

-- now, sometimes it so happens that both sides of `++` do occur together (for 
-- example, in the code walk-through above, `step 1` and `step 2` both produce 
-- results.), and when this happens, the parse is incomplete. for example, in 
-- the code below, `readsPrec` does not produce a singleton list; instead, you 
-- get a 2-element list, and it's second element reflects an incomplete parse.

--    readsPrec 0 "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: [(Tree Int, String)]
--      = [(Leaf 1 :^: Leaf 2 :^: Leaf 3, ""),
--         (Leaf 1, " :^: (Leaf 2 :^: Leaf 3)")]

-- `readsPrecT` below returns `readsTree` result if it is NOT an empty list; 
-- otherwise, it returns `readsLeaf` result. so in this way, `readsPrecT` makes 
-- `readsTree` & `readsLeaf` mutually exclusive, i.e., an `OR`. for below items, 
-- `readsPrecT` produces a singletion list that reflects a complete parse:

--    readsPrecT 0 "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: [(Tree Int, String)]
--      = [(Leaf 1 :^: Leaf 2 :^: Leaf 3,"")]

--    readsPrecT 0 "Leaf 1" :: [(Tree Int, String)]
--      = [(Leaf 1,"")]

--    readsPrecT 0 "Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))"
--      :: [(Tree Int, String)]
--      = [(Leaf 1 :^: Leaf 2 :^: Leaf 3 :^: Leaf 4,"")]

-- NOTE: `readsPrecT` can NOT parse a list of `Tree a`, as its return type is 
-- `Tree a`, not `[Tree a]`; ditto for a `Tree` tuple. on the other hand, since 
-- the return type of `readsPrec` is `Reads a`, it can return any `a`, including 
-- `Tree a`, or `[Tree a]`, or `(Tree a, Tree a)`. i also noticed that if you 
-- set `readsPrec = readsPrecT` in `Read` instance of `Tree`, then it indeed 
-- reads lists & tuples, not sure why though.

-- tested `readsPrecT` (by having it as `readsPrec`) for tuple & list:

--      read "(Leaf 1, Leaf 2 :^: Leaf 3)" :: (Tree Int, Tree Int)
--        = (Leaf 1,Leaf 2 :^: Leaf 3)

--      read "[Leaf 1, Leaf 2 :^: Leaf 3]" :: [Tree Int]
--        = [Leaf 1,Leaf 2 :^: Leaf 3]

-- i first saw this sort of `++` equivalence in `readsTree` implementations at 
-- https://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/stdclasses.html

-- NOTE:
--  1. NOT true that `readsTreeT` always produces a singleton list, even if one 
--     of the items in the list is a complete parse. in below example, we get a 
--     2-element list, which comes from `++` in the code `optional r = g r 
--     ++ mandatory r` in `readParen` invoked by `readsPrecT`.

--      readsPrecT 0 "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: [(Tree Int, String)]
--        = [((Leaf 1 :^: Leaf 2) :^: Leaf 3,""),
--           (Leaf 1 :^: Leaf 2," :^: Leaf 3")]

--  2. by the way, `readsPrec` gives identical result:

--      readsPrec 0 "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: [(Tree Int, String)]
--        = [((Leaf 1 :^: Leaf 2) :^: Leaf 3,""),
--           (Leaf 1 :^: Leaf 2," :^: Leaf 3")]

--  3. in point 1, how does `readsPrecT` produce a "bracket": i.e. `(Leaf 1 :^: 
--     Leaf 2)`?  well, this happens when `u` in `readsTree` itself parses to 
--     something like `_ :^: _`. a similiar thing, i suppose, can happen for `v` 
--     in `readsTree` as well.

--  4. what happens if we pass a "BAD" string to `readsPrecT`? well, it FAILS to 
--     parse it completely!

--        readsPrecT 0 "Leaf 1 :^: Leaf 2 :^: Leaf 3" :: [(Tree Int, String)]
--          = [(Leaf 1 :^: Leaf 2," :^: Leaf 3")]

--  5. ditto for `readsPrec`: it gives a 2-element list, both elements reflect 
--     incomplete or failed parses.

--        readsPrec 0 "Leaf 1 :^: Leaf 2 :^: Leaf 3" :: [(Tree Int, String)]
--          = [(Leaf 1 :^: Leaf 2," :^: Leaf 3"),
--             (Leaf 1," :^: Leaf 2 :^: Leaf 3")]

--  6. in an earlier version of `readsPrecT` code, it calls `readsLeaf` in 
--     `readsTree` for extracting the part before `:^:`. i thought this would 
--     work because the operator `:^:` is right associative, so the LHS of `:^:` 
--     is always a `Leaf`. BUT I WAS WRONG (see point 1 above)!!  for example, 
--     the below code fails to parse fully with this approach:

--        readsPrecT 0 "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: [(Tree Int, String)]
--          = [(Leaf 1 :^: Leaf 2," :^: Leaf 3")]
readsPrecT :: forall a. (Read a) => Int -> ReadS (Tree a)
readsPrecT d0 r0 = let a1 = readsTree
                       a2 = readsLeaf
                   in case (a1, a2) of
                      ([], [])     -> []
                      ((_:_), _)   -> a1
                      ([], (_:_))  -> a2
 where readsTree :: [(Tree a, String)]
       -- `(d0 > up_prec)` condition corresponds to the one in `showsPrec`.
       readsTree = readParen' (d0 > up_prec)
         (\r -> [(u:^:v, w) |
                 (u, s)     :: (Tree a, String) <- readsPrecT (up_prec+1) r,
                 (":^:", t) :: (String, String) <- lex s,
                 (v, w)     :: (Tree a, String) <- readsPrecT (up_prec+1) t])
         r0
       readsLeaf :: [(Tree a, String)]
       -- `(d0 > app_prec)` condition corresponds to the one in `showsPrec`.
       readsLeaf = readParen' (d0 > app_prec)
         (\r -> [(Leaf m, t) |
                 ("Leaf", s)  :: (String, String) <- lex r,
                 (m, t)       :: (a, String)      <- readsPrec (app_prec+1) s])
         r0
       app_prec = 10
       up_prec  = 5

--------------------------------------------------------------------------------
-- | `readParen'` is identical to `readParen` in haskell prelude.
-- using this code, instead of library code, allows me to debug the code in GHCi 
-- and see what is going on within `readParen`.
--
-- NOTE: `mandatory` parses enclosing parentheses by calling `lex` at start and 
-- end. between these 2 `lex` calls, `mandatory` calls `optional`, which parses 
-- using the function `g` (same as parser `p` passed to `readsPrec`). so 
-- `mandatory` parses what parser `p` parses but surrounded with parenthesis."
readParen'      :: forall a. Bool -> ReadS a -> ReadS a
readParen' b g  =  if b then mandatory else optional
                   where optional    :: ReadS a
                         optional r  = g r ++ mandatory r
                         mandatory   :: ReadS a
                         mandatory r = [(x,u) | ("(",s) :: (String, String) <- lex r,
                                                (x,t)   :: (a, String)      <- optional s,
                                                (")",u) :: (String, String) <- lex t ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | example 2: `Read` instance for `SomeType` that has custom `Show` instance.
-- REF: /u/ hvr @ https://tinyurl.com/2yk4br39 (so)

-- | `SomeType` type.
data SomeType a = Type a | Mix (SomeType a) (SomeType a) deriving Eq

-- | `Show` instance.
instance (Show a) => Show (SomeType a) where
  show (Type a) = show a
  show (Mix a b) = "(" ++ show a ++ " " ++ show b ++ ")"

-- | `Read` instance.

-- | how does `read "(3 4)"` parse?
-- see below for a run thru execution steps.
-- NOTE: see also GHCi DEBUG @ 
-- ../notes/chap2-data-types--Read--SomeType-Debug.lhs
-- readsPrec 0 "(3 4)"
--    readMix "(3 4)"
--    + readParen True g "(3 4)"
--    + + mandatory "(3 4)"
--    + + + lex ("(", ...) => PASS
--    + + + optional "3 4)"
--    + + + + g "3 4)"
--    + + + + + readsPrec 0 "3 4)"
--    + + + + + + readMix "3 4)"
--    + + + + + + + readParen True g "3 4)"
--    + + + + + + +   mandatory => FAILS in lex ("(", ...)
--    + + + + + + + FAIL
--    + + + + + + FAIL
--    + + + + + + readType "3 4)"
--    + + + + + + + readsPrec 0 "3 4)" => PASS
--    + + + + + + RETURNS (Type 3, " 4)")
--    + + + + + RETURNS (Type 3, " 4)")
--    + + + + + readsPrec 0 " 4)")
--    + + + + + + readMix " 4)"
--    + + + + + + + readParen True " 4)"
--    + + + + + + + + mandatory " 4)" => FAIL in lex ("(", ...)
--    + + + + + + + FAIL
--    + + + + + + FAIL
--    + + + + + + readType " 4)"
--    + + + + + + + readsPrec 0 " 4)" => PASS
--    + + + + + + RETURNS (Type 4, ")")
--    + + + + + RETURNS (Type 4, ")")
--    + + + + RETURNS (Mix Type 3 Type 4, ")")
--    + + + + mandatory "3 4)" => FAILS in lex ("(", ...)
--    + + + RETURNS (Mix Type 3 Type 4, ")")
--    + + + lex (")") => PASS
--    + + RETURNS (Mix Type 3 Type 4, "")
--    + RETURNS (Mix Type 3 Type 4, "")
--    RETURNS (Mix Type 3 Type 4, "")
-- ++ readType 0 "(3 4)"
--    + readsPrec 0 "(3 4)" => FAIL
--    FAIL
-- = [(Mix Type 3 Type 4, "")]
--
-- read "(3 4)" = Mix Type 3 Type 4
--
instance (Read a) => Read (SomeType a) where
  readsPrec d0 r0 = readMix r0 ++ readType r0
      where readMix :: String -> [(SomeType a, String)]
            -- NOTE: `showsPrec` does NOT use any precedence, yet it inserts 
            -- parenthesis for the `Mix` constructor, regardless of precedence.  
            -- so over here, we simply pass `True` to `readParen`, regardless of 
            -- precedence, because we know there will be parenthesis.
            readMix = readParen' True $ \r -> do
              (v1, r'') :: (SomeType a, String) <- readsPrec d0 r
              (v2, r')  :: (SomeType a, String) <- readsPrec d0 r''
              return (Mix v1 v2, r')
            readType   :: String -> [(SomeType a, String)]
            -- no call to `readParen` here as `showsPrec` does NOT insert 
            -- parenthesis for this part, regardless of the precedence level.
            readType r = do
              (v, r') :: (a, String) <- readsPrec d0 r  -- readsPrec for type `a`
              return (Type v, r')

-- | the definition below is equivalent to `readsPrec` for `SomeType` above.
-- example: readsPrecST 0 "(3 4)" :: [(SomeType Int, String)] = [((3 4),"")]
-- notice how the `++` in above definition translates to an `OR` pattern match 
-- below.  this is because `readMix` & `readType` in above definition are 
-- mutually exclusive: if one is successful, the other is not, and vice versa.

-- also, tested `readsPrecST` (by having it as `readsPrec`) for tuple & list.

-- i first saw this sort of `++` equivalence in `readsTree` implementations at 
-- https://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/stdclasses.html

-- NOTE: in an earlier version, called `readsPrec_`, i had code like this:
--      readsPrec_ d r | ("(":s) <- r = readMix_
--                     | otherwise    = readType_
-- but it failed (i.e., produced []) for the case below that had deep nesting:
--      show $ Mix (Mix (Type 4) (Mix (Type 5) (Type 6))) (Type 7)
--        = "((4 (5 6)) 7)"
readsPrecST :: forall a. (Read a) => Int -> ReadS (SomeType a)
readsPrecST d0 r0 = let a1 = readMix_ r0
                        a2 = readType_ r0
                    in case (a1, a2) of
                        ([], [])     -> []
                        ((_:_), _)   -> a1
                        ([], (_:_))  -> a2
    where readMix_ :: String -> [(SomeType a, String)]
          -- NOTE: `showsPrec` does NOT use any precedence, yet it inserts 
          -- parenthesis for the `Mix` constructor, regardless of precedence.  
          -- so over here, we simply pass `True` to `readParen`, regardless of 
          -- precedence, because we know there will be parenthesis.
          readMix_ = readParen' True $ \r -> do
            (v1, r'') :: (SomeType a, String) <- readsPrecST d0 r
            (v2, r')  :: (SomeType a, String) <- readsPrecST d0 r''
            return (Mix v1 v2, r')
          readType_   :: String -> [(SomeType a, String)]
          -- no call to `readParen` here as `showsPrec` does NOT insert 
          -- parenthesis for this part, regardless of the precedence level.
          readType_ r = do
            (v, r') :: (a, String) <- readsPrec d0 r  -- readsPrec for type `a`
            return (Type v, r')

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | example 3: `Read` instance of `TT` type that has a custom `Show` instance.
-- source: haskell 2010 report, chapter 11, @ https://tinyurl.com/2p833dbh
-- author: Prem Muthedath

-- | `TT` type.
infixr 4 :$
data TT = Int :$ TT  |  NT deriving Eq

-- | `Show` instance.
-- NOTE: show (1 :$ 2 :$ NT) should produce "1 :$ (2 :$ NT)". this is needed for 
-- correct parsing by `read`, as `show` and `read` are closely related.
instance Show TT where
  showsPrec p e0 = case e0 of
    x :$ y -> showChild 4 " :$ " x y
    NT     -> showString "NT"
    where showChild :: Int -> String -> Int -> TT -> ShowS
          showChild pr s x y =
            -- the code `p == pr` inserts parenthesis for part after `:$`.
            showParen (p > pr) $
              -- we use `11` because `x` is an `Int`.
              showsPrec 11 x . (s ++) .
              -- we up precedence to put parenthesis for the part after `:$`. of 
              -- course, if `y` is just `NT`, then no parenthesis is inserted; 
              -- see the code above.
              showsPrec (pr + 1) y

-- | `Read` instance.
-- NOTE: show (1 :$ 2 :$ NT) produces "1 :$ (2 :$ NT)". so we design `readsPrec` 
-- accordingly tp parse such expressions.
instance Read (TT) where
  readsPrec d r = readTT r ++ readNT r
    where readTT :: String -> [(TT, String)]
          -- `(d > op_prec)` condition corresponds to the one in `showsPrec`.
          readTT = readParen (d > op_prec) $ \r' -> do
              -- the parse below is for `Int`, so we use `11` as precedence.
              (u, s) :: (Int, String)       <- readsPrec 11 r'
              (":$", t) :: (String, String) <- lex s
              -- the parse below is for part after `:$` in `TT`, which if it has 
              -- an `:$`, it should be in parenthesis, so we up the precedence.  
              -- this agrees with `pr + 1` code in `showsPrec` (see above).
              (v, w) :: (TT, String)        <- readsPrec (op_prec + 1) t
              return (u :$ v, w)
          readNT :: String -> [(TT, String)]
          -- we do NOT call `readParen` for `NT` for any precedence because 
          -- `showsPrec` does NOT put parenthesis for `NT` for any precedence.
          readNT = \r' -> do
              ("NT", t) :: (String, String) <- lex r'
              return (NT, t)
          op_prec :: Int
          op_prec = 4

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | example 4: consider a data declaration that derives `Show`.
-- source: /u/ brian huffman @ https://tinyurl.com/4tdrxt72 (so)
-- we want to write custom `Read` instances for both `P` & `T`.

-- | Types `P` & `T`.
data P = P deriving Eq
data T = P :# P | T P deriving (Eq, Show)
infix 6 :#

-- | `Show` instance for `P`.
instance Show P where
  -- limit precedence to valid values: i.e., between 0 and 11.
  showsPrec p P | p `elem` [0 .. 11] = shows p
                | otherwise          = error "precdence > 11."

-- | Haskell's derived `Show` instance for `T` behaves like the code below:
-- source: /u/ brian huffman @ https://tinyurl.com/4tdrxt72 (so)
-- instance Show T where
--   showsPrec p e0 = case e0 of
--      T P    -> showParen (p > 10) $ showString "T " . showsPrec 11 P
--      P :# P -> showParen (p > 6) $ showsPrec 7 P . showString " :# " . showsPrec 7 P

-- | sample computations:
--    showsPrec 6 (P :# P) ""
--    = "7 :# 7"              -- calls `showsPrec 7` on arguments.
--    showsPrec 7 (P :# P) ""
--    = "(7 :# 7)"            -- shows parenthesis b'cause 7 > 6.
--    showsPrec 10 (T P) ""
--    = "T 11"
--    showsPrec 11 (T P) ""
--    = "(T 11)"              -- shows parenthesis b'cause 11 > 10.
--    show (P :# P)
--    = "7 :# 7"
--    show (T P)
--    = "T 11"

-- | `Read` instance for `P`.
-- NOTE: an equivalent code using `lex` & list comprehension:
--    readsPrec _ r = [(P, "") | (x, _) <- lex r, (all isDigit x) && valid x]
--      where valid :: String -> Bool
--            valid x = (read x :: Int) `elem` [0 .. 11]
instance Read P where
  readsPrec _ r = do
    -- we use `11` because we are reading an `Int`.
    (x, y) :: (Int, String) <- readsPrec 11 r
    guard $ x `elem` [0 .. 11]
    return (P, y)

-- | `Read` instance for `T`
instance Read T where
  readsPrec d r = readPP r ++ readT r
    where readPP :: String -> [(T, String)]
          -- NOTE: in general, `readParen` condition is needed to parse any 
          -- STARTING parenthesis inserted by `showsPrec`, because if any 
          -- STARTING parenthesis is there, we have to call `mandatory` in 
          -- `readParen` to parse the STARTING bracket.
          --
          -- now, usually you start off `readPrec` call with 0 precedence, so in 
          -- the first call, `mandatory` is never called; instead `optional` is 
          -- called, but within `optional`, there is an embedded call to 
          -- `mandatory`, which steps in to parse the STARTING bracket. so in 
          -- one way or the other, the code handles this condition.
          --
          -- but note that the reverse is NOT true: that is, if you call 
          -- `mandatory` (without going through `optional`) at the START when 
          -- the string has NO STARTING parenthesis, then the parse will fail. 
          -- to make sure that such a condition never happens, we need to ensure 
          -- that `readsPrec` has the SAME precedence as `showsPrec`:
          --
          -- (readsPrec 10 (showsPrec 5 (P :# P) "") :: [(T, String)])
          -- == [(P :# P, "")] => False
          --
          -- but if they have the SAME precedence, the parse will always 
          -- succeed:
          --
          --  (readsPrec 10 (showsPrec 10 (P :# P) "") :: [(T, String)])
          --  == [(P :# P, "")] => True
          --
          -- once we got the STARTING parenthesis, `mandatory` calls within 
          -- `optional` will handle any number of subsequent parenthesis.
          --
          -- (readsPrec 0 "(((7 :# 7)))" :: [(T, String)])
          -- == [(P :# P, "")] => True
          --
          --  (readsPrec 10 (showsPrec 10 ((((P :# P)))) "") :: [(T, String)]) 
          --  == [(P :# P, "")] => True
          ----------------------------------------------------------------------
          --  `(d > op_prec)` condition corresponds to the one in `showsPrec`.
          readPP = readParen (d > op_prec) $ \r' -> do
              -- since `showsPrec` uses `7`, we read using `7` as well.
              (P, s) :: (P, String)         <- readsPrec (op_prec + 1) r'
              (":#", t) :: (String, String) <- lex s
              (P, w) :: (P, String)         <- readsPrec (op_prec + 1) t
              return (P :# P, w)
          readT :: String -> [(T, String)]
          --  `(d > app_prec)` condition corresponds to the one in `showsPrec`.
          readT = readParen (d > app_prec) $ \r' -> do
              ("T", s) :: (String, String)  <- lex r'
              -- since `showsPrec` uses `11`, we use `11` as well here.
              (P, t) :: (P, String)         <- readsPrec (app_prec + 1) s
              return (T P, t)
          op_prec   = 6 :: Int
          app_prec  = 10 :: Int

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
