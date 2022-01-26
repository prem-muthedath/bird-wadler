{-# LANGUAGE ScopedTypeVariables #-}

-- | chapter 2: bird & wadler, introduction to functional programming.
-- `Read` class: example.
-- usage:
--  1. `cd` to `bird-wadler`, this package's top-level directory.
--  2. on commandline, run `cabal v2-repl :bird-wadler` to start GHCi.
--  3. next, at GHCi prompt, enter `import C2Read`.
--  4. you can then invoke any of the top-elevel functions.
--  5. OR instead of steps 2-4,
--      a) run `cabal v2-repl :bird-wadler-test` to start GHCi;
--      b) at GHCi prompt, enter `import C2ReadTest`;
--      c) invoke `C2ReadTest.ghciQC` to run all quickcheck tests.
-- author: Prem Muthedath, NOV 2021.

--------------------------------------------------------------------------------
module C2Read where
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
-- | example 1: `Read` instance for `Tree` that has a custom `Show` instance.
-- source: chapter 11, `specification of derived instances', `section 5`, 
-- haskell 2010 report @ https://tinyurl.com/nxh9d2y

infixr 5 :^:
data Tree a =  Leaf a  |  Tree a :^: Tree a deriving Eq

--------------------------------------------------------------------------------
-- | `Show` instance for `Tree`.
-- examples:
--    show (Leaf (1 :: Int) :^: Leaf 2 :^: Leaf 3 :^: Leaf 4)
--      = "Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))"
--    show ((Leaf 1 :^: Leaf 2) :^: Leaf (3 :: Int))
--      = "(Leaf 1 :^: Leaf 2) :^: Leaf 3"
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
         --
         -- what does this mean, by the way?? it means that even when right 
         -- associativity dictates no need for parenthesis, we still insert 
         -- parenthesis, if needed, both on the left & the right. that is, in 
         -- general, associativity is NOT used to reduce parenthesis; instead, 
         -- precedence IS used to reduce parenthesis.
         --
         -- in the example below, strictly speaking, because of right 
         -- associativity, no parenthesis is required, as the grouping from 
         -- right is obvious, but we ignore right associativity and instead 
         -- focus on precedence.
         --
         -- all instances of `:^:` in the argument expression for `show` have 
         -- the same precedence, so when we apply `show`, the outcome of `show` 
         -- should group stuff in such a way as to make the order clear to a 
         -- subsequent consumer of `show`'s output. this consumer is 
         -- `readsPrec`, which is required tp parse `showsPrec` output back to a 
         -- `Tree`. this strategy makes `showsPrec` & `readsPrec` compatable.  
         -- if you don't do this, `readsPrec` parse will fail or be incomplete!
         --
         -- in this example, because multiple instances of `:^:` all have the 
         -- same precedence, `show`'s output groups then from right, as it 
         -- should, because of right associativity, using right parenthesis. the 
         -- parenthesis makes clear the grouping of operators of same 
         -- precedence, even though the right associativity dictates that no 
         -- parenthesises are required (isn't the group obvious, joe?):
         --
         --   show (Leaf 1 :^: Leaf 2 :^: Leaf 3 :^: Leaf 4)
         --     = "Leaf 1 :^: (Leaf 2 :^: (Leaf 3 :^: Leaf 4))"

--------------------------------------------------------------------------------
-- | `Read` instance for `Tree`
instance (Read a) => Read (Tree a) where
  ------------------------------------------------------------------------------
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
  --  another example (to illustrate grouping):
  --      show ((Leaf 1 :^: Leaf 2) :^: Leaf (3 :: Int))
  --        = "(Leaf 1 :^: Leaf 2) :^: Leaf 3"
  --      (read "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: Tree Int)
  --        == (Leaf (1 :: Int) :^: Leaf 2 :^: Leaf 3)
  --        => False
  --      (read "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: Tree Int)
  --        == ((Leaf (1 :: Int) :^: Leaf 2) :^: Leaf 3)
  --        => True
  --      readsPrec 0 "(Leaf 1 :^: Leaf 2) :^: Leaf 3" :: [(Tree Int, String)]
  --        = [ ((Leaf 1 :^: Leaf 2) :^: Leaf 3,"") => `optional = g r`
  --          , Leaf 1 :^: Leaf 2," :^: Leaf 3")    => `optional = ++ mandatory 
  --          r` ]
  ------------------------------------------------------------------------------
  --`(d0 > up_prec)` condition corresponds to the one in `showsPrec`.
  -- note that we up the precedence to parse parts both to the left and to the 
  -- right of :^:. this is in line (symmetrical!) with code in `showsPrec`, and 
  -- its purpose is to parse the parenthesis that `showsPrec` may have put in.
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
-- below definition is equivalent to or better (??) than the `readsPrec` above.

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
-- the return type of `readsPrec` is `ReadS a`, it can return any `a`, including 
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
--
--  7. although i have claimed here that `readsPrecT` may be better than 
--     `readsPrec`, the `case` analysis in `readsPrecT` is simple enough because 
--     there is just 1 operator involved (i.e., :^:).  if there are multiple 
--     operators, the `case` analysis gets very complex, even non-doable, but 
--     the `++` technique used in `readsPrec` extends very well even to such 
--     situations. given this finding, i prefer the `readsPrec` algorithm.
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
-- | `Show` instance.
-- examples:
--    show $
--      Mix (Type (2 :: Int)) (Type (7 :: Int))
--      = "(2 7)"
instance (Show a) => Show (SomeType a) where
  show (Type a) = show a
  show (Mix a b) = "(" ++ show a ++ " " ++ show b ++ ")"
--------------------------------------------------------------------------------
-- | `Read` instance.
-- examples:
--     read "(3 4)" = Mix (Type 3) (Type 4)

-- | how does `read "(3 4)"` parse?
-- see below for a run thru execution steps.
-- NOTE: see also GHCi DEBUG @ ../notes/chap2-Read-SomeType-debug.lhs
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
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
-- | the definition below is equivalent to `readsPrec` for `SomeType` above.
-- example:
--    readsPrecST 0 "(3 4)" :: [(SomeType Int, String)]
--      = [(Mix (Type 3 Type 4),"")]
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
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
-- | `Show` instance.
-- examples:
--    show (1 :$ 2 :$ NT) should produce "1 :$ (2 :$ NT)".
--
-- in this case, right associativity dictates no parenthesis, as the grouping is 
-- "obvious", but we still insert parenthesis because `showsPrec` does not use 
-- associativity to reduce parenthesis; instead, its chief focus is precdence.
--
-- all operators in the expression above have same precedence, and for 
-- `readsPrec` to parse this string back to `TT`, `showsPrec` has to make the 
-- grouping clear using parenthesis. otherwise, `readsPrec` will fail.
instance Show TT where
  showsPrec p e0 = case e0 of
    x :$ y -> showChild 4 " :$ " x y
    NT     -> showString "NT"
    where showChild :: Int -> String -> Int -> TT -> ShowS
          showChild pr s x y =
            showParen (p > pr) $
              -- we use `11` because `x` is an `Int`.
              showsPrec 11 x . (s ++) .
              -- we up precedence to put parenthesis for the part after `:$`. of 
              -- course, if `y` is just `NT`, then no parenthesis is inserted; 
              -- see the code above.
              showsPrec (pr + 1) y
--------------------------------------------------------------------------------
-- | `Read` instance.
-- examples:
--    show (1 :$ 2 :$ NT) = "1 :$ (2 :$ NT)"
--    (read "1 :$ (2 :$ NT)" :: TT) = (1 :$ 2 :$ NT)
--    (read "1 :$ (2 :$ NT)" :: TT) = (1 :$ (2 :$ NT))
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
--------------------------------------------------------------------------------
-- | `Show` instance for `P`.
instance Show P where
  -- limit precedence to valid values: i.e., between 0 and 11.
  showsPrec p P | p `elem` [0 .. 11] = shows p
                | otherwise          = error "precedence > 11 or < 0."

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
--------------------------------------------------------------------------------
-- | `Read` instance for `P`.
-- examples:
--    (read "T 11" :: T) = T P
--    (read "7 :# 7" :: T) = P :# P
--    (read "(7 :# 7)" :: T) = P :# P
instance Read P where
  readsPrec _ r = do
    -- NOTE: an equivalent code using `lex` & list comprehension:
    --    readsPrec _ r = [(P, "") | (x, _) <- lex r, (all isDigit x) && valid x]
    --      where valid :: String -> Bool
    --            valid x = (read x :: Int) `elem` [0 .. 11]
    -- we use `11` because we are reading an `Int`.
    (x, y) :: (Int, String) <- readsPrec 11 r
    guard $ x `elem` [0 .. 11]
    return (P, y)
--------------------------------------------------------------------------------
-- | `Read` instance for `T`
instance Read T where
  readsPrec d r = readPP r ++ readT r
    where readPP :: String -> [(T, String)]
          --  NOTE: `readsPrec` parses a string, the output of `showsPrec`, to 
          --  extract a data structure, the original argument to `showsPrec`.  
          --  `readsPrec`, conceptually, is nothing but the exact reverse 
          --  operation of `showsPrec`, and it is guaranteed to work only for 
          --  inputs that are outputs of its corresponding `showsPrec` for the 
          --  SAME precedence.
          --
          --  in general, for nested data types, `readsPrec` algorithm involves 
          --  parsing the string either for a nested structure or a leaf. you 
          --  parse through nested structures until you get to a leaf: that is 
          --  the flow.  parentheses in `showsPrec` output "usually" enclose 
          --  nested structures, so correct parsing of parentheses and the 
          --  nested structures they enclose, using precedence, is key. that in, 
          --  essence, is what `readsPrec` is all about.
          --
          --  so how do you implement `readsPrec`? we got 2 inter-related 
          --  problems at hand: one, to parse parentheses correctly using 
          --  precedence; two, to recursively parse the string to extract 
          --  enclosed nested data structures. `readParen` call in `readsPrec` 
          --  code addresses the 1st problem; a combination of parsing functions 
          --  in `readsPrec` code addresses the 2nd problem. `readsPrec` 
          --  sequences these two  operations: the order depends on what the 
          --  "root" node is, but "usually" you first parse parentheses, then 
          --  enclosed structures.  and you do these operations repeatedly, 
          --  recursively, until you exhaust the string.
          --
          --  `readsPrec` usually calls `readParen` with a boolean condition on 
          --  precedence to parse any STARTING parentheses inserted by 
          --  `showsPrec`. if the condition is true, implying presence of 
          --  parentheses, `readParen` calls `mandatory`, a local function, to 
          --  parse any starting parentheses and the nested data structures they 
          --  enclose.  otherwise, `readParen` calls `optional`, another local 
          --  function, to parse the string. note that parsing algorithm walks 
          --  the string left to right, parsing it piece by piece, so "STARTING" 
          --  refers to the start of any string piece fed to parse.
          --
          --  in general, `readsPrec` parse for a recursive type consists of 
          --  2 parts: one for the composite & the other for the leaf.
          --
          --  `readsPrec` (usual defn) = composite parse ++ leaf parse.
          --
          --  the `++` acts, ideally, and more often, as an `OR`, rather than an 
          --  `AND`; that is, if one part fails, you still get a parse value 
          --  from the other part, and vice versa. and usually it is the case 
          --  that one part fails and the other succeeds, and that both 
          --  simultaneously do not succeed. however, if, for some reason, both 
          --  parts return values, then you get a concatenation. of course, if 
          --  both parts simultaneously fail, then you get [], a parse failure.
          --
          --  the key part is the design of the composite parse. the composite 
          --  parse function must be recursive, walking through the entire 
          --  supplied string and repeatedly parsing it to generate the nested 
          --  structure. this dictates a parse function like the one below.
          --    do
          --      <- `readsprec` pr r  => say, function `f`
          --      <- `lex`             => say, function `g`
          --      <- `readsPrec` pr r  => say, function `h`
          --  this kind of structure allows recursive parsing of both left 
          --  (i.e., before the `lex`) and right (i.e., after the `lex`) sides 
          --  of the operator to extract nested structures. the composite parse 
          --  succeeds only if all 3 steps in `do` succeed.
          --
          --  this composite parsing function (sometimes the leaf parser as 
          --  well) is usually passed to `readParen`, which calls `optional`, in 
          --  one way or the other, with this function as argument. this is how 
          --  `readsPrec` relates these 2 parts of its job.
          --
          --  our goal is to parse the string and extract the data structure.  
          --  this could involve parsing any parentheses and the nested stuff 
          --  they enclose. so what is the parse strategy? well, the string 
          --  parse always proceeds left to right, but for the parse to be 
          --  successful (i.e., exhaustive), the parse must first "center" 
          --  itself on the "root" node -- the outermost or top node.  this top 
          --  node may be a parenthesis (if, for example, the entire string is 
          --  within parentheses) or an operator. `showsPrec` usually inserts 
          --  parentheses to make the top node clear. anyway, the parse should 
          --  be centered on the top node; if not, you will get a bad parse.
          --
          --  to center the parse on the top node, one sure way is to force 
          --  `readParen` to call `optional` in the very first pass. to do this, 
          --  usually, you start off `readsPrec` call with 0 precedence, so in 
          --  the 1st call, `mandatory` is never called; instead `optional` is 
          --  called, but you structure your `readsPrec` code in such a way that 
          --  recursive calls to `readsPrec` from `optional` uses a higher 
          --  precedence to force `readParen` to invoke `mandatory`, instead of 
          --  `optional`, in subsequent passes.  `mandatory` then parses any 
          --  parentheses, as well as nested structures enclosed.  you have to 
          --  call `mandatory` after the first pass, otherwise you can not parse 
          --  nested structures, which are typically enclosed in parentheses.
          --
          --  to do this, within `do`, we set `pr = op_prec + 1`, where 
          --  `op_prec` is the operator precedence, for `readsPrec` calls in `f` 
          --  & `g`, so that `readsPrec` calls in `f`, & `g`, invoked from 
          --  `optional`, will invoke `mandatory` in `readParen`. this step is 
          --  also important to avoid infinite recursion.  if you don't do this 
          --  step, then `readsPrec` calls in `f` & `g` will invoke `optional` 
          --  once again with the same argument & precedence, which will invoke 
          --  `optional` again, and so on ... and the calls will never end.
          --
          --  invoking `mandatory`, as described, in a secondary pass will parse 
          --  any starting parentheses.  `mandatory` will then invoke `optional` 
          --  with a different string to parse the enclosed nested structure.  
          --  `optional` will once again, through `readsPrec` calls in `f` & 
          --  `g`, invoke `mandatory`.  `mandatory` will again parse any 
          --  starting parentheses and call `optional` ... and so on. this 
          --  sequence will be repeated until you reach a leaf or you have a 
          --  parse failure (returns []) for some reason. if the `optional` call 
          --  succeeds, `mandatory` will parse the closing parenthesis and 
          --  return the entire parse result.  if there is no starting 
          --  parenthesis to begin with, then the first call to `mandatory` 
          --  (from `optional`, remember?) will return [], and the entire parse 
          --  will terminate, as it should.
          --
          --  `mandatory` parse usually fails when you have a leaf, because 
          --  leaves have no parentheses. despite this failure, `readsPrec` 
          --  calls, through the `++` mentioned above, will return a leaf.
          --
          --  so why does this implementation work? and why can't we call 
          --  `mandatory` at the very start, instead of calling `optional`?  
          --  well, calling `mandatory`, instead of `optional`, at the start 
          --  does not seem to always work. for instance, the parse will fail if 
          --  the string does not begin with parenthesis.
          --
          --  even if you have starting parenthesis, calling `mandatory` first, 
          --  without at first going through `f` & `g` calls in `optional`, does 
          --  not ensure success. here is an example from `Tree`, where the 2nd 
          --  list element (an incomplete parse) comes from `mandatory` call:
          --
          --      show ((Leaf 1 :^: Leaf 2) :^: Leaf (3 :: Int))
          --        = "(Leaf 1 :^: Leaf 2) :^: Leaf 3"
          --      readsPrec 0 "(Leaf 1 :^: Leaf 2) :^: Leaf 3"
          --      :: [(Tree Int, String)]
          --      = [ ((Leaf 1 :^: Leaf 2) :^: Leaf 3,"") => `optional = g r`
          --        , Leaf 1 :^: Leaf 2," :^: Leaf 3")    => `= ++ mandatory r`
          --        ]
          --
          --  so why does this happen? well, let us look at how the recursive 
          --  parse happens. for a given string, the parse always goes from left 
          --  to right, but not all left-to-right parses succeed.  to be 
          --  successful, the parse, before doing anything else, first needs to 
          --  "center" itself on the "root node" or the top node. doing so also 
          --  enables subsequent recursive calls to automatically center 
          --  themselves on the root nodes of the nested structures they handle. 
          --  this combination enables the entire parse to succeed. if you don't 
          --  do this, the parse will be incomplete or return [].
          --
          --  as you can see below, although there are 2 `:^:` operators in the 
          --  below structure, only one of them is the "root" or top node:
          --
          --     ( Leaf 1
          --       :^:           => calling `mandatory` first parses this node.
          --       Leaf 2
          --     )
          --     :^:             => "root operator" or top node.
          --     Leaf 3
          --
          --  so let us see how the `optional` parse works on this structure.
          --    1. when `optional` is called first, it first "centers" the parse 
          --       on `:^:`. next, it calls `f` & `g` functions to parse the 
          --       left & right sides of `:^:`.
          --    2. `readsPrec` in `f` calls `readParen` with a new precedence, 
          --       which forces `readParen` to call `mandatory`.
          --    3. `mandatory` begins the parse from left, & parses "(", the 
          --       enclosed nested structure, & ")", returning (`Leaf 
          --       1 :^: Leaf 2`, ":^: Leaf 3").
          --    4. `lex` parses remaining string, returning (":^:", "Leaf 3").
          --    5. next, `readsPrec` in `g` calls `readParen` with a new 
          --       precedence to parse the remaining string returned by `lex`.
          --    6. `readParen` calls `mandatory` to do this parse. `mandatory` 
          --       begins the "(" parse from left, but there is no starting 
          --       parenthesis, so that parse fails, returning [].
          --    7. `readsPrec` in `g`, through `++`, calls the leaf parser, 
          --       which returns (`Leaf 3`, "").
          --    8. the entire result of the complete parse (`(Leaf 1 :^: Leaf 
          --       2) :^: Leaf 3`, "") is then returned.
          --
          --  now let us look at the second case, where `mandatory` is first 
          --  called, without going through `f` &  `g` calls in `optional`.
          --    1. the `optional` call in step 1 in the previous analysis also 
          --       has a `mandatory` call coming from:
          --          `optional r = `f'` r ++ mandatory r`
          --    2. in the steps just listed above, we looked at how the parse 
          --       happens with f' r.  now we look at the part after `++`.
          --    3. `mandatory`begins the parse from left, parses "(", then calls 
          --       `optional` to parse the remaining string.
          --    4. `optional` centers the parse on `:^:` and invokes `f` & `g`.
          --    5. `readsPrec` in `f` calls `readParen` with a new precedence, 
          --       which forces `readParen` to call `mandatory`.
          --    6. `mandatory` begins the parenthesis parse, but there is no 
          --       "(", so the parse fails, returning [].
          --    7. `readsPrec` in `f` then calls the leaf parser (through `++`), 
          --       returning (`Leaf 1`, ":^: Leaf 2) :^: Leaf 3")
          --    8. `lex` (remaining) => (":^:", "Leaf 2) :^: Leaf 3".
          --    9. `readsPrec` in `g` then parses the remaining string, just 
          --       like steps 4, 5, & 6, returning (`Leaf 2`, ") :^: Leaf 3").
          --    10. `mandatory` (step 3) parses ")", giving (")", ":^: Leaf 3".
          --    11. the returned parse result will be (`Leaf 
          --        1 :^: Leaf 2`, ":^: Leaf 3"), an incomplete parse.
          --
          --  as stated before, correct parse requires that it first "centers" 
          --  itself on the "root" node. when we call `optional` first, it first 
          --  centers the parse on `:^:` (step 1) & then calls `f` & `g` to 
          --  parse left & right sides of the "root" node. `f` & `g`, through 
          --  `readsPrec`, then calls `mandatory` to parse parentheses and 
          --  nested structures on the left & right, giving a complete parse.
          --
          --  note that when `optional` first centers the parse on `:^:`, it has 
          --  no idea where the root node is, or even which of the two `:^:` is 
          --  the root/top node, but the fact that one of the two `:^:` is the 
          --  root node invariably makes the selected `:^:` the root node 
          --  because of the very nature of how the parse proceeds.
          --
          --  on the other hand, if you call `mandatory` first, it parses the 
          --  starting parenthesis "(" first (step 3), but the starting 
          --  parenthesis "(" is NOT the top node, so you end up with an 
          --  incomplete parse.
          --
          --  calling `mandatory` first will only work if the entire string is 
          --  enclosed in parentheses.  here is an example from `Tree1`:
          --
          --      show $
          --        Branch1 (Leaf1 "foo") (Leaf1 "goo")
          --        = "<\"foo\"|\"goo\">"
          --
          --      (readsPrec 0 "<\"foo\"|\"goo\">"
          --        :: [(Tree1 String, String)])
          --        = [( Branch1 (Leaf1 "foo") (Leaf1 "goo"), "")]
          --
          --  in the above example, we call `mandatory` first, and the parse 
          --  succeeds, because `mandatory` begins the parse from "<", the top 
          --  node. "<" is the top node because the entire string is enclosed in 
          --  angluar brackets -- always; see `show` output listed above.
          --
          --  but even in this example, calling `optional` first will succeed, 
          --  because as pointed out earlier, `optional` has a `++ mandatory r` 
          --  component to parse "optional" brackets, and since the outer 
          --  brackets enclosing the string are "optional", in some sense, this 
          --  `mandatory` component handles the outer brackets.
          --
          --  so in summary, whether or not you have parentheses, calling 
          --  `optional` first will always succeed for recursive structures. as 
          --  stated above, you do this by first calling `readPrec` with 0 
          --  precedence, and in subsequent recursive `readsPrec` calls, you up 
          --  the precedence to force `mandatory` calls.  this strategy works 
          --  -- always -- for recursive structures.
          --
          --  there is another situation you should be aware of where 
          --  `mandatory` may be invoked at the start in an incorrect manner.  
          --  suppose your `readsPrec` code is good, but you made the cardinal 
          --  mistake of invoking `readsPrec` with a DIFFERENT precedence than 
          --  `showsPrec`, you could then end up invoking `mandatory` at the 
          --  start, and most likely, your parse will be incorrect:
          --
          --    (readsPrec 10 (showsPrec 5 (P :# P) "") :: [(T, String)])
          --      == [(P :# P, "")] => False
          --
          --  but if they have SAME precedence, the parse will succeed:
          --
          --    (readsPrec 10 (showsPrec 10 (P :# P) "") :: [(T, String)])
          --      == [(P :# P, "")] => True
          --
          --  once we got the STARTING parenthesis, any subsequent redundant or 
          --  "optional"  parentheses will be parsed by the `++ mandatory r` 
          --  component in `optional`, whether or not you call `optional` first.
          --
          --    (readsPrec 0 "(((7 :# 7)))" :: [(T, String)])
          --      == [(P :# P, "")] => True
          --
          --    (readsPrec 10 (showsPrec 10 ((((P :# P)))) "")
          --      :: [(T, String)])
          --      == [(P :# P, "")] => True
          --
          --  so in summary the composite part of `readsPrec` is "usually" 
          --  designed in such a way that you start off with 0 precedence, so in 
          --  the first pass you can not not handle any starting parenthesis.  
          --  instead, you use the first pass to center your parse on the "root" 
          --  node, and then you handle parentheses in subsequent recursive 
          --  `readsPrec` calls by suddenly upping the precedence. the "root" 
          --  node, as shown above, can be either an operator or a parenthesis.
          --
          --  to enable a successful parse, the structure of `readsPrec` usually 
          --  mirrors the structure of `showsPrec`, especially the parts 
          --  handling precedence and therefore parenthesis. i mention this 
          --  detail because it is important to design `showsPrec` in such a way 
          --  to output string with parenthesis at right places (using 
          --  precedence) that can be parsed by `readsPrec`, using the design 
          --  discussed above. parentheses indicate (usually) nested structures, 
          --  and having parenthesis at right places in the string fed to 
          --  `readsPrec` will allow successful recursive parse using precedence 
          --  values.  if not, you can get bad parses or stack overflow.
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
-- | example 5: `Read` instance for `Expr`.
-- source: /u/ brian huffman @ https://tinyurl.com/4tdrxt72 (so)

-- | `Expr` type declaration.
data Expr =
  Const Int |
  Expr :+: Expr |
  Expr :-: Expr |
  Expr :*: Expr |
  Expr :/: Expr deriving Eq
--------------------------------------------------------------------------------
-- | operator associativity & precedence.
infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:
--------------------------------------------------------------------------------
-- | custom `Show` instance.
-- source: /u/ brian huffman @ https://tinyurl.com/4tdrxt72 (so)
-- examples:
--    show (Const 1 :+: Const 2 :*: Const 3 :+: Const 4)
--      = "(Const 1 :+: (Const 2 :*: Const 3)) :+: Const 4"
--    show
--      (Const 1 :/: Const 2 :-: (Const (-1) :+: Const 2))
--      = "Const 1 :/: Const 2 :-: (Const (-1) :+: Const 2)"
--
-- NOTE: `Show` instance code is from /u/ brian huffman @ so (link above), but i 
-- could not easily write a `readsPrec` to parse his `showsPrec` output, because 
-- brian's `showsPrec` does NOT ignore associativity. so i changed his code to 
-- ignore associativity. now `showsPrec` ups precedence on both sides of the 
-- operator, as opposed to just one side in brian's code, resulting in 
-- parenthesis even when associativity dictates otherwise.
instance Show Expr where
  showsPrec p e0 = case e0 of
     Const n -> showParen (p > 10) $ showString "Const " . showsPrec 11 n
     x :+: y -> showParen (p > 6) $ showsPrec 7 x . showString " :+: " . showsPrec 7 y
     x :-: y -> showParen (p > 6) $ showsPrec 7 x . showString " :-: " . showsPrec 7 y
     x :*: y -> showParen (p > 7) $ showsPrec 8 x . showString " :*: " . showsPrec 8 y
     x :/: y -> showParen (p > 7) $ showsPrec 8 x . showString " :/: " . showsPrec 8 y
--------------------------------------------------------------------------------
-- | `Read` instance.
-- author: Prem Muthedath.
--------------------------------------------------------------------------------
-- | examples:
--    readsPrec 0
--      "(Const 1 :+: (Const 2 :*: Const 3)) :+: Const 4"
--      :: [(Expr, String)]
--      = Const 1 :+: Const 2 :*: Const 3 :+: Const 4
--    show (Const 1 :+: Const 2 :*: Const 3 :+: Const 4)
--      = "(Const 1 :+: (Const 2 :*: Const 3)) :+: Const 4"
--    (read "(Const 1 :+: (Const 2 :*: Const 3)) :+: Const 4"
--      :: Expr) =
--      Const 1 :+: Const 2 :*: Const 3 :+: Const 4
--
-- | another example:
--    show
--      (Const 1 :/: Const 2 :-: (Const (-1) :+: Const 2))
--      = "Const 1 :/: Const 2 :-: (Const (-1) :+: Const 2)"
--    readExp1 0
--      "Const 1 :/: Const 2 :-: (Const (-1) :+: Const 2)"
--      :: [(Expr, String)]
--      = []
--    readExp2 0
--      "Const 1 :/: Const 2 :-: (Const (-1) :+: Const 2)"
--      :: [(Expr, String)]
--      = [(Const 1 :/: Const 2 :-: (Const (-1) :+: Const 2),"")]
--    readExp3 0
--      "Const 1 :/: Const 2 :-: (Const (-1) :+: Const 2)"
--      :: [(Expr, String)]
--      = []
--    readExp4 0
--      "Const 1 :/: Const 2 :-: (Const (-1) :+: Const 2)"
--      :: [(Expr, String)]
--      = [(Const 1 :/: Const 2," :-: (Const (-1) :+: Const 2)")]
--    readConst 0
--      "Const 1 :/: Const 2 :-: (Const (-1) :+: Const 2)"
--      :: [(Expr, String)]
--      = [(Const 1, " :/: Const 2 :-: (Const (-1) :+: Const 2)")]
--------------------------------------------------------------------------------
-- NOTE: as stated before, though all parses walk the string left to right, the 
-- successful parse centers itsel first on the "root" node.
--
-- unlike previous cases, where we had just 1 operator, here we have 4 
-- operators, and it is impossible to know beforehand which of these in any 
-- given string is the root node. so we try all options, which is what `++`, 
-- ideally an `OR`, in the code below represent.
--
-- likewise, when parsing the left & right sides of any operator, again it is 
-- impossible to know which operator is the "root" node in the nested structure, 
-- so we use `++` to try all options.
--
-- in the example above, `:-:` is the "root" node, which is why `readExp2` gives 
-- a complete parse, while others give either [] (failed) or incomplete parses.
--------------------------------------------------------------------------------
instance Read Expr where
  readsPrec d r = readExp1 r
                  ++ readExp2 r
                  ++ readExp3 r
                  ++ readExp4 r
                  ++ readConst r
    where readExp1 :: ReadS Expr
          readExp1 = readParen (d > 6)
              (\r' -> [(u:+:v, w) |
                 (u, s)     :: (Expr, String)   <- readsPrec 7 r',
                 (":+:", t) :: (String, String) <- lex s,
                 (v, w)     :: (Expr, String)   <- readsPrec 7 t])
          readExp2 :: ReadS Expr
          readExp2 = readParen (d > 6)
              (\r' -> [(u:-:v, w) |
                 (u, s)     :: (Expr, String)   <- readsPrec 7 r',
                 (":-:", t) :: (String, String) <- lex s,
                 (v, w)     :: (Expr, String)   <- readsPrec 7 t])
          readExp3 :: ReadS Expr
          readExp3 = readParen (d > 7)
              (\r' -> [(u:*:v, w) |
                 (u, s)     :: (Expr, String)   <- readsPrec 8 r',
                 (":*:", t) :: (String, String) <- lex s,
                 (v, w)     :: (Expr, String)   <- readsPrec 8 t])
          readExp4 :: ReadS Expr
          readExp4 = readParen (d > 7)
              (\r' -> [(u:/:v, w) |
                 (u, s)     :: (Expr, String)   <- readsPrec 8 r',
                 (":/:", t) :: (String, String) <- lex s,
                 (v, w)     :: (Expr, String)   <- readsPrec 8 t])
          readConst :: ReadS Expr
          readConst = readParen (d > 10)
              (\r' -> [(Const m, t) |
                 ("Const", s) :: (String, String) <- lex r',
                 (m, t)       :: (Int, String)    <- readsPrec 11 s])
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | example 6: `Read` instance for `Tree1`.
-- source: /u/ Sven-Olof Nyström @ https://tinyurl.com/jmt7thd5 (uppsala univ)

-- | `Tree1` type.
data Tree1 a = Leaf1 a | Branch1 (Tree1 a) (Tree1 a) deriving Eq
--------------------------------------------------------------------------------
-- | `showsTree1`.
showsTree1                :: (Show a) => Tree1 a -> ShowS
showsTree1 (Leaf1 x)      =  shows x
showsTree1 (Branch1 l r)  =
    ('<':) . showsTree1 l . ('|':) . showsTree1 r . ('>':)

-- | `Show` instance for `Tree1`.
-- examples:
--  show $
--    Branch1 (Leaf1 "foo") (Leaf1 "goo")
--    = "<\"foo\"|\"goo\">"
--  show $
--    Branch1 (Leaf1 "foo") (Branch1 (Leaf1 "glob") (Leaf1 "hog"))
--    = <"foo"|<"glob"|"hog">>
--  show $
--    Branch1 (Branch1 (Leaf1 "glob") (Leaf1 "hog")) (Leaf1 "foo")
--    = "<<\"glob\"|\"hog\">|\"foo\">"
instance Show a => Show (Tree1 a) where
  show t= showsTree1 t ""
--------------------------------------------------------------------------------
-- | `Read` instance for `Tree1`.
-- examples:
--    (readsPrec 0 "<\"foo\"|\"goo\">"
--      :: [(Tree1 String, String)])
--      == [( Branch1 (Leaf1 "foo") (Leaf1 "goo"), "")]
--      => True
--    (readsPrec 0 "<\"foo\"|<\"glob\"|\"hog\">>"
--      :: [(Tree1 String, String)])
--      == [(Branch1 (Leaf1 "foo") (Branch1 (Leaf1 "glob") (Leaf1 "hog")), "")]
--      => True
--    (readsPrec 0 "<<\"glob\"|\"hog\">|\"foo\">"
--      :: [(Tree1 String, String)])
--      == [(Branch1 (Branch1 (Leaf1 "glob") (Leaf1 "hog")) (Leaf1 "foo"), "")]
--      => True
--
-- | swedish implementation:
--      source: /u/ Sven-Olof Nyström
--            @ https://tinyurl.com/jmt7thd5 (uppsala univ)
--      readsTree         :: (Read a) => ReadS (Tree a)
--      readsTree ('<':s) =
--        [ (Branch l r, u) |
--          (l, '|':t) <- readsTree s
--        , (r, '>':u) <- readsTree t
--        ]
--      readsTree s =
--        [(Leaf x, t)  | (x,t)   <- reads s]
--      instance Read a => Read (Tree a) where
--        readsPrec _ s = readsTree s
instance (Read a) => Read (Tree1 a) where
  -- since `showsTree1` does not use precedence, we don't use it here as well.
  -- note that `readTree1` & `readLeaf1` are mutually exclusive, as they should 
  -- be; compare this iimplementation with the swedish one listed above.
  readsPrec _ r = readTree1 r ++ readLeaf1 r
          -- | `readTree1` reads the composite structure.
          -- since `showTree1` encloses everything in angluar brackets by 
          -- default, regardless of precedence, in `readTree1`, we always call 
          -- `readAngle` with `True` to ensure bracket parsing.
          --
          -- NOTE: by passing `True` to `readAngle`, we force it to call 
          -- `mandatory` to parse the brackets first. this works fine here 
          -- because brackets form the outermost lexeme, the "top" nodes, as 
          -- `showsPrec` encloses everything in brackets. if brackets didn't 
          -- enclose everything, calling `mandatory` first woould yield an 
          -- incomplete parse.
          --
          -- on the other hand, calling `optional` in `readAngle` first will 
          -- always work, whether or not you have brackets. suppose, instead of 
          -- parsing brackets first, `readsPrec d` called `readAngle` first with 
          -- something like `(d > 0)`, with `d` = 0, `readAngle` will then call 
          -- `optional` first, instead of `mandatory`, but the parse would still 
          -- succeed, because `optional` always calls `mandatory` (see `++ 
          -- mandatory` in `optional` code) to parse any "optional" brackets 
          -- (outermost brackets are "optional").
          --
          -- by the way, the same `++ mandatory` in `optional` code always 
          -- parses any redundant (i.e., "optional") brackets in the supplied 
          -- string, whether or not you called `mandatory` first.
    where readTree1 :: ReadS (Tree1 a)
          readTree1 = readAngle True $
            \r' ->
              [ (Branch1 u v, w) |
                -- since `showTree1` does not use precedence, we use 11 here.
                (u, s)    :: (Tree1 a, String) <- readsPrec 11 r'
              , ("|", t)  :: (String, String)  <- lex' s
              , (v, w)    :: (Tree1 a, String) <- readsPrec 11 t
              ]
          -- | `readLeaf1` reads the leaf.
          readLeaf1 :: ReadS (Tree1 a)
          readLeaf1 =
            \r' -> [ (Leaf1 m, t) |
                     (m, t)  :: (a, String) <- readsPrec 11 r'
                   ]

-- | `readAngle` just like `readParen` but reads `>` & `<` instead.
readAngle :: forall a. (Read a) => Bool -> ReadS (Tree1 a) -> ReadS (Tree1 a)
readAngle b g = if b then mandatory else optional
  where -- | `optional` same as `optional` in `readParen`.
        optional :: ReadS (Tree1 a)
        -- the `++ mandatory r'` parses "optional" or redundant brackets.
        optional r' = g r' ++ mandatory r'
        -- | `mandatory` same as `mandatory` in `readParen`, except that it 
        -- parses angular brackets, instead of parentheses.
        mandatory :: ReadS (Tree1 a)
        mandatory r' =
          [ (u, v) |
            ("<", s)  :: (String, String)  <- lex' r'
          , (u, t)    :: (Tree1 a, String) <- optional s
          , (">", v)  :: (String, String)  <- lex' t
          ]

-- | `lex'`: given a string, returns a 1-element string tuple list.  tuple's 1st 
-- element is string's head; 2nd, string's tail. ignores leading spaces.
-- i wrote `lex'` because haskell's `lex` can NOT reliably parse 
-- multi-occurrence of haskell characters, such as `>`, `<`, `|`, used here. for 
-- example, since "<<" is a haskell lexeme, `lex "<<" /= [("<", "<")].
lex' :: String -> [(String, String)]
lex' []       = []
lex' (' ':xs) = lex' xs
lex' (x:xs)   = [([x], xs)]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | example 7: `Read` instance for `Time`.
-- source: /u/ andrewc @ https://tinyurl.com/3yv83hf3 (so)
-- `readsPrec` partly authored by Prem Muthedath.

data Time = Time {hour :: Int, minute :: Int} deriving Eq
--------------------------------------------------------------------------------
-- | `Show` instance for `Time`.
-- example:
--    show $ Time 10 15 = "10:15"
instance Show Time where
  show (Time h m) =
      ( if h > 9
          then (show h)
          else ("0" ++ show h))
      ++ ":"
      ++ ( if m > 9
              then (show m)
              else ("0" ++ show m))
--------------------------------------------------------------------------------
-- | `Read` instance for `Time`.
-- example:
--    (readsPrec 0 "10:15" :: [(Time, String)])
--    == [(Time 10 15, "")]
--    => True
instance Read Time where
  -- we do not care for precedence, since `show` does not use it.
  readsPrec _ r =
    [ (newTime h m, t) |    -- while reading, make sure we have a valid `Time`.
      -- NOTE: this parse does not allow spaces before ':', such as "10  :15".
      -- to allow spaces, rewrite using `lex'` defined earlier to parse ':'.
      (h, ':':s)  :: (Int, String)  <- readsPrec 11 r
    , (m, t)      :: (Int, String)  <- readsPrec 11 s
    ]
    where newTime :: Int -> Int -> Time
          newTime h m | between 0 23 h && between 0 59 m = Time h m
                      | otherwise = error "newTime: hours must be in range 0-23 and minutes 0-59"
          between :: Int -> Int -> Int -> Bool
          between low high val = low <= val && val <= high

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
