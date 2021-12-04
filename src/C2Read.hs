{-# LANGUAGE ScopedTypeVariables #-}

-- | chapter 2: bird & wadler, introduction to functional programming.
-- `Read` class: example.
-- usage: load this file in GHCi & invoke any top-level `test...` function.
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
--------------------------------------------------------------------------------
import Test.QuickCheck
import Control.Monad (liftM, liftM2)

--------------------------------------------------------------------------------
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
  readsPrec d0 r0 = readParen' (d0 > up_prec)
                      (\r -> [(u:^:v, w) |
                             (u, s)     :: (Tree a, String) <- readsPrec (up_prec+1) r,
                             (":^:", t) :: (String, String) <- lex s,
                             (v, w)     :: (Tree a, String) <- readsPrec (up_prec+1) t])
                      r0
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
       readsTree = readParen' (d0 > up_prec)
         (\r -> [(u:^:v, w) |
                 (u, s)     :: (Tree a, String) <- readsPrecT (up_prec+1) r,
                 (":^:", t) :: (String, String) <- lex s,
                 (v, w)     :: (Tree a, String) <- readsPrecT (up_prec+1) t])
         r0
       readsLeaf :: [(Tree a, String)]
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
-- | run `read`, `readsPrec`, `readsPrecT` on strings to yield `Tree` values.
testTree :: IO ()
testTree = do
    putStr "1. read \"Leaf 1 :^: (Leaf 2 :^: Leaf 3)\" = "
    print (read "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: Tree Int)
    putStr "2. read \"Leaf 1\" = "
    print (read "Leaf 1" :: Tree Int)
    putStr "3. readsPrec 0 \"Leaf 1\" = "
    print (readsPrec 0 "Leaf 1" :: [(Tree Int, String)])
    putStr "4. readsPrecT 0 \"Leaf 1\" = "
    print (readsPrecT 0 "Leaf 1" :: [(Tree Int, String)])
    putStr "5. readsPrec 0 \"Leaf 1 :^: (Leaf 2 :^: Leaf 3)\" = "
    print (readsPrec 0 "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: [(Tree Int, String)])
    putStr "6. readsPrecT 0 \"Leaf 1 :^: (Leaf 2 :^: Leaf 3)\" = "
    print (readsPrecT 0 "Leaf 1 :^: (Leaf 2 :^: Leaf 3)" :: [(Tree Int, String)])
    putStr "7. readsPrec 0 \"Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))\" = "
    print (readsPrec 0 "Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))" :: [(Tree Int, String)])
    putStr "8. readsPrecT 0 \"Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))\" = "
    print (readsPrecT 0 "Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))" :: [(Tree Int, String)])
    putStr "9. read \"(Leaf 1, Leaf 2 :^: Leaf 3)\" = "
    print (read "(Leaf 1, Leaf 2 :^: Leaf 3)" :: (Tree Int, Tree Int))
    putStr "10. read \"[Leaf 1, Leaf 2 :^: Leaf 3]\" = "
    print (read "[Leaf 1, Leaf 2 :^: Leaf 3]" :: [Tree Int])
    putStr "11. read \"(Leaf 1 :^: Leaf 2) :^: Leaf 3\" = "
    print (read "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: Tree Int)
    putStr "12. readsPrec 0 \"[(Leaf 1 :^: Leaf 2) :^: Leaf 3]\" = "
    print (readsPrec 0 "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: [(Tree Int, String)])
    putStr "13. readsPrecT 0 \"(Leaf 1 :^: Leaf 2) :^: Leaf 3\" = "
    print (readsPrecT 0 "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: [(Tree Int, String)])
    putStr "14. readsPrec 0 \"Leaf 1 :^: Leaf 2 :^: Leaf 3\" = "
    print (readsPrec 0 "Leaf 1 :^: Leaf 2 :^: Leaf 3" :: [(Tree Int, String)])
    putStr "15. readsPrecT 0 \"Leaf 1 :^: Leaf 2 :^: Leaf 3\" = "
    print (readsPrecT 0 "Leaf 1 :^: Leaf 2 :^: Leaf 3" :: [(Tree Int, String)])

--------------------------------------------------------------------------------
-- | QuickCheck tests.

-- | `Arbitrary` instance for `Tree`.
instance (Read a, Show a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized genTree

-- | helper functions for implementing arbitrary.
-- | generator for Tree type.
genTree :: forall a. (Read a, Show a, Arbitrary a) => Int -> Gen (Tree a)
genTree 0 = liftM Leaf (arbitrary :: Gen a)
genTree n = frequency [
      (1, liftM Leaf (arbitrary :: Gen a)),
      (4, liftM2 (:^:) (genTree (n `div` 2)) (genTree (n `div` 2)))
    ]

-- | quickcheck property to test `Tree` read.
prop_readTree :: (Int -> ReadS (Tree Int)) -> Property
prop_readTree f = forAll (arbitrary :: Gen (Tree Int)) $
  \x -> classify (isLeaf x) "leaf" $
        classify (depthT x > 3) "depth > 3" $
        classify (ldepthT x > 3) "left depth > 3" $
        classify (rdepthT x > 3) "right depth > 3" $
        (x,"") `elem` (f 0 (showsPrec 0 x ""))

-- | some helper functions.
isLeaf :: Tree a -> Bool
isLeaf (Leaf _) = True
isLeaf _        = False

depthT :: Tree a -> Int
depthT (Leaf _)  = 1
depthT (l :^: r) = 1 + max (depthT l) (depthT r)

ldepthT :: Tree a -> Int
ldepthT (Leaf _)  = 1
ldepthT (l :^: _) = 1 + ldepthT l

rdepthT :: Tree a -> Int
rdepthT (Leaf _)  = 1
rdepthT (_ :^: r) = 1 + rdepthT r

-- | quickcheck property to test `Tree` list read.
prop_readTreeList :: Property
prop_readTreeList = forAll (genList :: Gen [Tree Int]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         -- | NOTE: you can NOT substitute `readsPrecT` for `readsPrec` in below 
         -- line of code, as `readPrecT` returns `ReadS (Tree a)`, not `ReadS 
         -- [Tree a]`.  on the other hand, `readsPrec` returns `ReadS a`, so it 
         -- can handle `ReadS [Tree a]` as well. if you want to use `readsPrecT` 
         -- for reading lists, you have to set `readsPrec = readsPrecT` in the 
         -- `Read` instance for `Tree`.  i am not clear why such an equivalence 
         -- works, but it indeed does!
         (xs,"") `elem` (readsPrec 0 (showsPrec 0 xs ""))

-- | generate a random list of type `a`.
-- for `<$>` and `<*>`, see https://tinyurl.com/42h7z9vn (so)
-- ((:) <$> (arbitrary :: Gen a))
-- :: Gen ([a] -> [a])
-- (((:) <$> (arbitrary :: Gen a)) <*> myList)
-- :: Gen [a]
genList :: forall a. (Read a, Show a, Arbitrary a) => Gen [a]
genList = frequency
  [ (1, return [])
  , (4, ((:) <$> (arbitrary :: Gen a)) <*> genList)
  ]

-- | quickcheck property to test `Tree` tuple read.
prop_readTreeTuple :: Property
prop_readTreeTuple = forAll (genTuple :: Gen (Tree Int, Tree Int)) $
  \(x, y) -> classify (isLeaf x) "fst is leaf" $
             classify (isLeaf y) "snd is leaf" $
             classify (depthT x > 3) "fst depth > 3" $
             classify (depthT y > 3) "snd depth > 3" $
             -- | NOTE: you can NOT substitute `readsPrecT` for `readsPrec` in 
             -- below line of code, as `readPrecT` returns `ReadS (Tree a)`, not 
             -- `ReadS (Tree a, Tree a)`.  on the other hand, `readsPrec` 
             -- returns `ReadS a`, so it can handle `ReadS (Tree a, Tree a)` as 
             -- well.  if you want to use `readsPrecT` for reading tuples, you 
             -- have to set `readsPrec = readsPrecT` in `Read` instance for 
             -- `Tree`.  i am unclear why that equivalence works!
             ((x, y),"") `elem` (readsPrec 0 (showsPrec 0 (x, y) ""))

-- | generate a random 2-tuple of type `a`.
genTuple :: forall a. (Read a, Show a, Arbitrary a) => Gen (a, a)
genTuple = do
  x1 <- arbitrary :: Gen a
  y1 <- arbitrary :: Gen a
  return (x1, y1)

-- | `Tree` QuickCheck test cases.
treeTC :: [(String, Property)]
treeTC = [
           ("readsPrec tree",
            prop_readTree (readsPrec :: (Int -> ReadS (Tree Int)))
           ),
           ("readsPrecT tree",
            prop_readTree (readsPrecT :: (Int -> ReadS (Tree Int)))
           ),
           ("readsPrec tree list",
            prop_readTreeList
           ),
           ("readsPrec tree tuple",
            prop_readTreeTuple
           )
        ]

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
-- NOTE: see also the GHCi DEBUG @ ./chap2-data-types--Read--SomeType-Debug.hs
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
            readMix = readParen' True $ \r -> do
              (v1, r'') :: (SomeType a, String) <- readsPrec d0 r
              (v2, r')  :: (SomeType a, String) <- readsPrec d0 r''
              return (Mix v1 v2, r')
            readType   :: String -> [(SomeType a, String)]
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
          readMix_ = readParen' True $ \r -> do
            (v1, r'') :: (SomeType a, String) <- readsPrecST d0 r
            (v2, r')  :: (SomeType a, String) <- readsPrecST d0 r''
            return (Mix v1 v2, r')
          readType_   :: String -> [(SomeType a, String)]
          readType_ r = do
            (v, r') :: (a, String) <- readsPrec d0 r  -- readsPrec for type `a`
            return (Type v, r')

-- | run `read`, `readsPrec`, `readsPrecST` on strings to get `SomeType` values.
testSomeType :: IO ()
testSomeType = do
  putStr "1. read \"(3 4)\" = "
  print (read "(3 4)" :: SomeType Int)
  putStr "2. read \"4\" = "
  print (read "4" :: SomeType Int)
  putStr "3. readsPrec 0 \"(3 4)\" = "
  print (readsPrec 0 "(3 4)" :: [(SomeType Int, String)])
  putStr "4. readsPrecST 0 \"(3 4)\" = "
  print (readsPrecST 0 "(3 4)" :: [(SomeType Int, String)])
  putStr "5. readsPrec 0 \"4\" = "
  print (readsPrec 0 "4" :: [(SomeType Int, String)])
  putStr "6. readsPrecST 0 \"4\" = "
  print (readsPrecST 0 "4" :: [(SomeType Int, String)])
  -- | show $ Mix (Mix (Type 4) (Mix (Type 5) (Type 6))) (Type 7)
  --    = "((4 (5 6)) 7)"
  putStr "7. read \"((4 (5 6)) 7)\" = "
  print (read "((4 (5 6)) 7)" :: SomeType Int)
  -- | show $ Mix (Mix (Type 4) (Mix (Type 5) (Type 6))) (Type 7)
  --    = "((4 (5 6)) 7)"
  putStr "8. readsPrecST \"((4 (5 6)) 7)\" = "
  print (readsPrecST 0 "((4 (5 6)) 7)" :: [(SomeType Int, String)])
  putStr "9. read \"((3 4), 5)\" = "
  print (read "((3 4), 5)" :: (SomeType Int, SomeType Int))
  putStr "10. read \"[(3 4), 5]\" = "
  print (read "[(3 4), 5]" :: [SomeType Int])

--------------------------------------------------------------------------------
-- | QuickCheck tests.

-- | `Arbitrary` instance for `SomeType`.
instance (Read a, Show a, Arbitrary a) => Arbitrary (SomeType a) where
  arbitrary = sized genSomeType

-- | helper functions for implementing arbitrary.
-- | generator for SomeType type.
genSomeType :: forall a. (Read a, Show a, Arbitrary a) => Int -> Gen (SomeType a)
genSomeType 0 = liftM Type (arbitrary :: Gen a)
genSomeType n = frequency [
      (1, liftM Type (arbitrary :: Gen a)),
      (4, liftM2 Mix (genSomeType (n `div` 2)) (genSomeType (n `div` 2)))
    ]

-- | quickcheck property to test `SomeType` read.
prop_readSomeType :: (Int -> ReadS (SomeType Int)) -> Property
prop_readSomeType f = forAll (arbitrary :: Gen (SomeType Int)) $
  \x -> classify (isType x) "Type" $
        classify (depthST x > 3) "depth > 3" $
        classify (ldepthST x > 3) "left depth > 3" $
        classify (rdepthST x > 3) "right depth > 3" $
        (x,"") `elem` (f 0 (showsPrec 0 x ""))

-- | some helper functions.
isType :: SomeType a -> Bool
isType (Type _) = True
isType _        = False

depthST :: SomeType a -> Int
depthST (Type _)  = 1
depthST (Mix l r) = 1 + max (depthST l) (depthST r)

ldepthST :: SomeType a -> Int
ldepthST (Type _)  = 1
ldepthST (Mix l _) = 1 + ldepthST l

rdepthST :: SomeType a -> Int
rdepthST (Type _)  = 1
rdepthST (Mix _ r) = 1 + rdepthST r

-- | quickcheck property to test `SomeType` list read.
prop_readSomeTypeList :: Property
prop_readSomeTypeList = forAll (genList :: Gen [SomeType Int]) $
  \xs -> classify (xs==[]) "empty" $
         classify (length xs == 1) "have 1 element" $
         classify (length xs > 1) "have > 1 element" $
         -- | NOTE: you can NOT substitute `readsPrecT` for `readsPrec` in below 
         -- line of code, as `readPrecT` returns `ReadS (SomeType a)`, not 
         -- `ReadS [SomeType a]`.  on the other hand, `readsPrec` returns `ReadS 
         -- a`, so it can handle `ReadS [SomeType a]` as well. if you want to 
         -- use `readsPrecT` for reading lists, you have to set `readsPrec = 
         -- readsPrecT` in the `Read` instance for `SomeType`.  i am not clear 
         -- why such an equivalence works, but it indeed does!
         (xs,"") `elem` (readsPrec 0 (showsPrec 0 xs ""))

-- | quickcheck property to test `SomeType` tuple read.
prop_readSomeTypeTuple :: Property
prop_readSomeTypeTuple = forAll (genTuple :: Gen (SomeType Int, SomeType Int)) $
  \(x, y) -> classify (isType x) "fst is leaf" $
             classify (isType y) "snd is leaf" $
             classify (depthST x > 3) "fst depth > 3" $
             classify (depthST y > 3) "snd depth > 3" $
             -- | NOTE: you can NOT substitute `readsPrecT` for `readsPrec` in 
             -- below line of code, as `readPrecT` returns `ReadS (SomeType a)`, 
             -- not `ReadS (SomeType a, SomeType a)`.  on the other hand, 
             -- `readsPrec` returns `ReadS a`, so it can handle `ReadS (SomeType 
             -- a, SomeType a)` as well.  if you want to use `readsPrecT` for 
             -- reading tuples, you have to set `readsPrec = readsPrecT` in 
             -- `Read` instance for `Tree`. unclear why that equivalence works!
             ((x, y),"") `elem` (readsPrec 0 (showsPrec 0 (x, y) ""))

-- | `SomeType` QuickCheck test cases.
someTypeTC :: [(String, Property)]
someTypeTC = [
               ("readsPrec SomeType",
                prop_readSomeType (readsPrec :: (Int -> ReadS (SomeType Int)))
               ),
               ("readsPrecST SomeType",
                prop_readSomeType (readsPrecST :: (Int -> ReadS (SomeType Int)))
               ),
               ("readsPrec SomeType list",
                prop_readSomeTypeList
               ),
               ("readsPrec SomeType tuple",
                prop_readSomeTypeTuple
               )
            ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | run `QuickCheck` tests on all `Read` instances.
runAllQC :: IO ()
runAllQC = mapM_ runQC tests
  where -- | run `QuickCheck` test case.
        runQC :: (String, Property) -> IO ()
        runQC (x, y) = do
             putStrLn $ "--- " <> x <> " ---"
             quickCheck y
        tests :: [(String, Property)]
        tests = treeTC ++ someTypeTC

--------------------------------------------------------------------------------
