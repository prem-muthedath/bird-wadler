-- | chapter 2: bird & wadler, introduction to functional programming.
-- `Show` class: example.
-- usage: load this file in GHCi & invoke any top-level `test...` function.
-- Prem Muthedath, 22 OCT 2021.

--------------------------------------------------------------------------------
module C2Show where

-- | GHC.Show
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Show.html#v:show
-- see also haskell 2010 report, chapter 6, @ https://tinyurl.com/85e22dus
-- see also haskell 2010 report, chapter 11, @ https://tinyurl.com/2p833dbh
-- see also haskell 2010 report, chapter 9, @ https://tinyurl.com/2p8wdy3c

-- class Show a where
--    showsPrec :: Int -> a -> ShowS
--    show :: a -> String
--    showList :: [a] -> ShowS

-- Mimimal complete definition:
--    show or showsPrec

-- | ShowS type.
-- type ShowS = String -> String

-- | showsPrec: 1st arg is operator precedence (0-11) of enclosing context; 
-- function app has precedence 10; 2nd arg is value to be converted to `String`.
-- default implementation (below) works for all precedences.
-- showsPrec       :: Int -> a -> ShowS
-- showsPrec _ x s = show x ++ s

-- | show function.
-- show      :: a   -> String
-- show x    = shows x ""

-- | showList allows programmers to give a specialized way to show lists.
-- showList       :: [a] -> ShowS
-- showList ls  s = showList__ shows ls s

-- | showList__ function (called from showList)
-- showList__                 :: (a -> ShowS) ->  [a] -> ShowS
-- showList__ _     []     s  = "[]" ++ s
-- showList__ showx (x:xs) s  = '[' : showx x (showl xs)
--   where showl    :: [a] -> String
--     showl []     = ']' : s
--     showl (y:ys) = ',' : showx y (showl ys)

-- | equivalent to 'showsPrec' with a precedence of 0.
-- shows           :: (Show a) => a -> ShowS
-- shows           =  showsPrec 0

-- | utility function that surrounds the inner show function with
-- parentheses when the 'Bool' parameter is 'True'.
-- showParen       :: Bool -> ShowS -> ShowS
-- showParen b p   =  if b then showChar '(' . p . showChar ')' else p

-- | utility function converting a 'String' to a show function that
-- simply prepends the string unchanged.
-- showString      :: String -> ShowS
-- showString      =  (++)

-- | utility function converting a 'Char' to a show function that
-- simply prepends the character unchanged.
-- showChar        :: Char -> ShowS
-- showChar        =  (:)

-- | utility function to show space.
-- showSpace :: ShowS
-- showSpace = {- showChar ' '-} \ xs -> ' ' : xs

--------------------------------------------------------------------------------
-- | example 1: `showsPrec`, `showParen`, `showString` usage.
-- source: chapter 11, `specification of derived instances', `section 5`,
-- haskell 2010 report @ https://tinyurl.com/nxh9d2y

-- NOTE: in the code below, haskell computes `showsPrec n`, where `n` is an 
-- integer, using the `showsPrec` for `Integer` defined in `instance Show 
-- Integer` in `GHC.Show`:
--    instance Show Integer where
--        showsPrec p (IS i) r = showsPrec p (I# i) r
--        showsPrec p n r
--            | p > 6 && n < 0 = '(' : integerToString n (')' : r)
--            -- Minor point: testing p first gives better code
--            -- in the not-uncommon case where the p argument
--            -- is a constant
--            | otherwise = integerToString n r
--        showList = showList__ (showsPrec 0)

infixr 5 :^:
data Tree a =  Leaf a  |  Tree a :^: Tree a

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

-- SAMPLE COMPUTATION (to do this GHCi, please run `testTree` given below):
--    show (Leaf 1 :^: Leaf 2 :^: Leaf 3)
--    = "Leaf 1 :^: (Leaf 2 :^: Leaf 3)"

-- | run `show` on a bunch of `Tree` types.
testTree :: IO ()
testTree = mapM_ putStrLn computations
  where computations = [
            "1. Leaf 1 :^: (Leaf 2 :^: Leaf 3)",
            "= " <> show (Leaf 1 :^: Leaf 2 :^: Leaf 3),
            "2. (Leaf 1 :^: Leaf 2) :^: Leaf 3",
            "= " <> show ((Leaf 1 :^: Leaf 2) :^: Leaf 3),
            "3. [Leaf 15, Leaf 6 :^: (Leaf 7 :^: Leaf 8)]",
            "= " <> show ([Leaf 15, Leaf 6 :^: Leaf 7 :^: Leaf 8]),
            "4. (Leaf 1 :^: Leaf 2 :^: Leaf 3, Leaf 5 :^: (Leaf 6 :^: Leaf 7))",
            "= " <> show ((Leaf 1 :^: Leaf 2 :^: Leaf 3, Leaf 5 :^: Leaf 6 :^: Leaf 7))
          ]

--------------------------------------------------------------------------------
-- | example 2: how does `showsPrec` work when we `derive` `Show`?  And how can 
-- we use it to make a custom `showsPrec`?  Well, below examples explains it.

-- | first, general rules ( haskell report 2010 @ https://tinyurl.com/nxh9d2y ):
--    i)    `showsPrec` deals with precedence, not with associativity.
--    ii)   `showsPrec` accepts a precedence level `d` and a value `x`.
--    iii)  precedence level is a number from 0 to 11.
--    iv)   `showsPrec` d x r ++ s = `showsPrec` d x (r ++ s)
--    v)    the representation will be enclosed in parenthesis if the precedence 
--          of the top-level constructor in `x` is < `d`.
--    vi)   if `d` is 0, then the result is never surrounded in parenthesis.
--    vii)  if `d` is 11, then the result is always surrounded in parenthesis, 
--          unless it is an atomic expression.
--    viii) `show` results in a syntactically correct haskell expression 
--          containing constants. by default, parenthesis are only added where 
--          needed, IGNORING associativity. the results of `show` are readable 
--          by `read` if all component types are readable.
--    ix)   derived instances of `Show` & `Read` obey the following:
--            a) if constructor is an infix operator, then derived `Read` 
--               instance will parse only infix applications of constructor;
--            b) associativity is not used to reduce parenthesis, although 
--               precedence may be. for example,
--                  infixr 4 :$
--                  data T = Int :$ T | NT
--
--                  show (1 :$ 2 :$ NT)        => "1 :$ (2 :$ NT)"
--                  read "1 :$ (2 :$ NT)" :: T => succeeds
--                  read "1 :$ 2 :$ NT" :: T   => fails
--    x)    in a nutshell, `showsPrec` does exactly what its name says: it 
--          shows, where needed, precedence of an expression by enclosing it in 
--          parenthesis, just as we would, where needed, enclose an expression 
--          within parenthesis by hand.
--
--  haskell compiler uses both associativity and precedence to parse 
--  expressions. `showsPrec` on the other hand addresses just precedence, not 
--  associativity. but we can define `showsPrec` (as a hack) to handle limited 
--  associativity for the purpose of inserting parenthesis.
--
--  in general, `showsPrec p (op (...))`, where `p` is enclosing context 
--  precedence & `op` is current context operator, involves 2 decisons;
--    a) is `p > precedence of 'op'`? if yes, then the entire expression `op 
--       (...)` is enclosed within parenthesis. otherwise, not.
--    b) how do we deal with args to `op` when we call `showsPrec` on them?  
--       what precedence must we specify in that call? surely, whatever 
--       precedence we specify in that call will affect decision `a)` noted 
--       above on parenthesizing. below, we review guidelines/rules from 2 
--       sources -- Richard Bird and /u/ brian huffman from so -- that state the 
--       same thing in essence but in different ways.
--
-- | example ("thinking functionally in haskell", chapter 11, richard bird):
--      showsPrec p (Bin op e1 e2)
--          = showParen (p>q) (showsPrec q e1 . showSpace .
--            showsop op . showSpace . showsPrec (q+1) e2)
--            where q = prec op
--
--      "We put parentheses around an expression if the parent operator has 
--      greater precedence than the current one. To display the expression e1 it 
--      is therefore sufficient to pass the current precedence as the new parent 
--      precedence. But we need parentheses around e2 if the root operator of e2 
--      has precedence less than or equal to q; so we have to increment q in the 
--      second call."
--
--      Bird (chapter 11, page 294) codifies rules for when we need parenthesis 
--      ... consider `e1 op e2` where `op` is left associative ...
--        a) `(e1)` => if `op` is `*` or `/` AND root of `e1` is `+` or `-`
--        b) `(e2)` => if `op` is `*` or `/` AND root of `e2` is `+,-,*,/`
--        c) `(e2)` => if `op` is `+` or `-` AND root of `e2` is `+` or `-`
--
--      so we can generalize parenthesis rules for left-associative `op`:
--        i) parenthesis on `e1` if `e1` root precedence < `op` precedence;
--        ii) parenthesis on `e2` if `e2` root precedence <= `op` precedence.
--
-- | example: /u/ brian huffman @ https://tinyurl.com/4tdrxt72 (so)
--      /u/ brian states the following rules, but they are not a standard by any 
--      means; for example, in the `Tree` defn above, with `infixr`, `showsPrec` 
--      still calls both args with (n+1).  Ditto if we derive `Show`.
--
--      NOTE: per /u/ brian, the rules BELOW work fine as long as you do NOT 
--      have `infixr` and `infixl` constructors at the SAME precedence level.
--
--        1. infix n    : use showParen (p > n), showsPrec (n+1) on the left, 
--           and showsPrec (n+1) on the right;
--        2. infixl n   : use showParen (p > n), showsPrec n on the left, and 
--           showsPrec (n+1) on the right;
--        3. infixr n   : use showParen (p > n), showsPrec (n+1) on the left, 
--           and showsPrec n on the right;
--        4. non-infix  : use showParen (p > 10) and showsPrec 11 on the 
--           arguments.

-- | consider first a data declaration that derives `Show`.
-- source: /u/ brian huffman @ https://tinyurl.com/4tdrxt72 (so)
data P = P
data T = P :# P | T P deriving Show
infix 6 :#

instance Show P where
  showsPrec p P = shows p

-- sample computations (to do this in GHCi, please run `testT` given below):
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

-- | run `showsPrec` and `show` on a bunch of type `T`.
testT :: IO ()
testT = mapM_ putStrLn computations
  where computations :: [String]
        computations = [
            "showsPrec 6 (P :# P) \"\"",
            "= " <> showsPrec 6 (P :# P) "",
            "showsPrec 7 (P :# P) \"\"",
            "= " <> showsPrec 7 (P :# P) "",
            "showsPrec 10 (T P) \"\"",
            "= " <> showsPrec 10 (T P) "",
            "showsPrec 11 (T P) \"\"",
            "= " <> showsPrec 11 (T P) "",
            "show (P :# P)",
            "= " <> "7 :# 7",
            "show (T P)",
            "= " <> "T 11"
          ]

-- | now consider an example where we define a customized `Show` instance:
-- source: /u/ brian huffman @ https://tinyurl.com/4tdrxt72 (so)
data Expr =
  Const Int |
  Expr :+: Expr |
  Expr :-: Expr |
  Expr :*: Expr |
  Expr :/: Expr

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:

-- using the above-mentioned rules, we can define a custom `showsPrec`:
instance Show Expr where
  showsPrec p e0 = case e0 of
     Const n -> showParen (p > 10) $ showString "Const " . showsPrec 11 n
     x :+: y -> showParen (p > 6) $ showsPrec 6 x . showString " :+: " . showsPrec 7 y
     x :-: y -> showParen (p > 6) $ showsPrec 6 x . showString " :-: " . showsPrec 7 y
     x :*: y -> showParen (p > 7) $ showsPrec 7 x . showString " :*: " . showsPrec 8 y
     x :/: y -> showParen (p > 7) $ showsPrec 7 x . showString " :/: " . showsPrec 8 y

-- some computations (to do this in GHCi, please run `testExp` given below):
--    show (Const 1 :+: Const 2 :*: Const 3 :+: Const 4)
--    = "Const 1 :+: Const 2 :*: Const 3 :+: Const 4"
--    show ((Const 1 :+: Const 2) :*: (Const 3 :+: Const 4))
--    = "(Const 1 :+: Const 2) :*: (Const 3 :+: Const 4)"
--    show (Const 1 :+: Const 2 :-: Const 3 :-: Const 4)
--    = "Const 1 :+: Const 2 :-: Const 3 :-: Const 4"
--    show (Const 1 :+: Const 2 :-: (Const 3 :-: Const 4))
--    = "Const 1 :+: Const 2 :-: (Const 3 :-: Const 4)"

-- | run `show` on a bunch of `Expr` types.
-- NOTES:
--  1.  `show (Const 1 :+: Const 2 :*: Const 3 :+: Const 4)` becomes:
--        a) `showsPrec 0 (Const 1 :+: Const 2 :*: Const 3 :+: Const 4)`
--        b) which is parsed as (because of left associativity):
--            x :+: y
--            x :: Expr = :+: (Const 1) (:*: (Const ...) (Const ...))
--            y :: Expr = Const 4
--  2.  `show (Const 1 :+: Const 2 :-: Const 3 :-: Const 4)` becomes:
--        a) `showsPrec 0 (Const 1 :+: Const 2 :-: Const 3 :-: Const 4)`
--        b) which is parsed as (because of left associativity):
--            x :-: y
--            x :: Expr = :-: (:+: (Const 1) (Const 2)) (Const 3)
--            y :: Expr = Const 4
--  3.  `show (Const 1 :+: Const 2 :-: (Const 3 :-: Const 4))` becomes:
--        a) `showsPrec 0 (Const 1 :+: Const 2 :-: (Const 3 :-: Const 4))`
--        b) which becaomes (because of left associativity, parenthesis):
--            x :-: y
--            x :: Expr = :+: (Const 1) (Const 2)
--            y :: Expr = :-: (Const 3) (Const 4)
--  4.  `show ((Const 1 :+: Const 2) :*: (Const 3 :+: Const 4))` becomes:
--        a) `showsPrec 0 ((Const 1 :+: Const 2) :*: (Const 3 :+: Const 4))`
--        b) which is parsed as (because of parenthesis):
--            x :*: y
--            x :: Expr = :+: (Const 1) (Const 2)
--            y :: Expr = :-: (Const 3) (Const 4)
--  5.  in cases (1) & (2), where the parent root is `:+:` or `:-:`, we end up 
--      on the RHS (i.e., `y`) either something like `Const n` or an expression 
--      of higher precedence than its parent, such as `:*: (...) (...)`, neither 
--      of which needs parenthesis. also, rule 1 (see above) handles y = `Const 
--      n` with `p > 10`, so it is almost never in parenthesis (& need not be).
--  6.  however, in case (3), where the parent root is `:-:`, we've on the RHS 
--      (i.e., `y`) `:-: (Const 3) (Const 4)`, which is an expression needing
--      parenthesis because it is at the same level of precedence as its parent 
--      operator `:-:`. so to get RHS parenthesis, we need to up the precedence 
--      (i.e., from 6 to 7) passed to `showsPrec p y`. this is what bird's rules 
--      & /u/ brian's rule 2 (see above) do, & that's why they work!
--  7.  note that in (2), we start with `:-:` at the top, then on the LHS (i.e., 
--      `x`), we once again have `:-:`, which has the same precedence as its 
--      parent `:-:`, but this is not a problem at all because `:-:` is left 
--      associative, so we don't need to show parenthesis.  however, as noted in 
--      (6), we need parenthesis when a similiar situation arises on the RHS.
--  8.  in the case discussed in point (7), derived `Show` instance may still 
--      insert parenthesis on LHS, even though they are redundant, because the 
--      derived `show` does not use associativity to reduce parenthesis.
--  9.  in case (4), where the parent is `:*:` with precedence `p`, on LHS 
--      (i.e., `x`), we have `:+:`, which has a lower precedence than `:*:`, so 
--      we need parenthesis on `x`, which we will get if we call `showsPrec p 
--      x`. likewise, on RHS (i.e., `y`), we have `:-:`, which also has a lower 
--      precedence than `:*:`, so we need parenthesis on `y` as well, which 
--      we'll get if we call `showsPrec p y`. but point (6) solution -- 
--      `showsPrec (p+1) y` -- is more general, so we'll go with that for RHS.
--  10. the key idea while using `showsPrec` is to avoid redundant 
--      parenthesises; for example, if we have an expression on RHS with a 
--      higher precedence than its parent, then enclosing it in parenthesis is 
--      redundant in case of left associativity operators. on the other hand, 
--      again for left associativity, if root of RHS expression is at the same 
--      or lower level precedence as its parent, then `showsPrec` needs to 
--      enclose it in parenthesis. rules cited above from bird & /u/ brian 
--      ensure these things.
testExp :: IO ()
testExp = mapM_ putStrLn computations
  where computations :: [String]
        computations = [
            "1. Const 1 :+: Const 2 :*: Const 3 :+: Const 4",
            "= " <> show (Const 1 :+: Const 2 :*: Const 3 :+: Const 4),
            "2. (Const 1 :+: Const 2) :*: (Const 3 :+: Const 4)",
            "= " <> show ((Const 1 :+: Const 2) :*: (Const 3 :+: Const 4)),
            "3. Const 1 :+: Const 2 :-: Const 3 :-: Const 4",
            "= " <> show (Const 1 :+: Const 2 :-: Const 3 :-: Const 4),
            "4. Const 1 :+: Const 2 :-: (Const 3 :-: Const 4)",
            "= " <> show (Const 1 :+: Const 2 :-: (Const 3 :-: Const 4)),
            "5. (Const 1 :*: (Const 2 :-: (Const 3 :*: (Const 4 :/: Const 5))))",
            "= " <> show (Const 1 :*: (Const 2 :-: (Const 3 :*: (Const 4 :/: Const 5)))),
            "6. [Const 1, Const 2 :*: (Const 3 :+: Const 4), Const 5 :*: Const 6]",
            "= " <> show ([Const 1, Const 2 :*: (Const 3 :+: Const 4), Const 5 :*: Const 6]),
            "7. (Const 1 :+: Const 2, Const 3 :-: (Const 4 :*: Const 5))",
            "= " <> show ((Const 1 :+: Const 2, Const 3 :-: (Const 4 :*: Const 5)))
          ]

-- | another shot at the `Expr` problem, following /u/ myzoski @ 
-- https://tinyurl.com/4tdrxt72 (so).
-- this one does not follow rules stated above from /u/ brian explicitly, but in 
-- essence boils down to the same thing. the nice thing about this formulation 
-- is that it makes clear the seperation between enclosing (i.e., parent) 
-- context and current (i.e., child) context in precdence comparisions.

-- | `L` (for language) is same as `Expr` defined above.
data L =
  C Int |
  L ::+:: L |
  L ::-:: L |
  L ::*:: L |
  L ::/:: L

infixl 6 ::+::
infixl 6 ::-::
infixl 7 ::*::
infixl 7 ::/::

-- | returns precedence of a given `L`.
prec :: L -> Int
prec (C _) = 10
prec (_ ::*:: _) = 7
prec (_ ::/:: _) = 7
prec (_ ::+:: _) = 6
prec (_ ::-:: _) = 6

-- | `Show` instance for `L`.
-- notice that in `showChild`, we insert parenthesis if parent's precedence is > 
-- than child's. also, we insert parenthesis when `y`'s (i.e., RHS) precedence 
-- is = current precedence to display bracketted expressions on RHS.
-- note that in `y`'s case, we only do `prec y == pr` in `showChild` because the 
-- case of `prec y < pr` is already taken care in `showsPrec pr y`.
instance Show L where
  showsPrec p e0 = case e0 of
     C n -> showParen (p > 10) $ showString "C " . showsPrec 11 n
     x ::+:: y -> showChild 6 " ::+:: " x y
     x ::-:: y -> showChild 6 " ::-:: " x y
     x ::*:: y -> showChild 7 " ::*:: " x y
     x ::/:: y -> showChild 7 " ::/:: " x y
     where showChild :: Int -> String -> L -> L -> ShowS
           showChild pr s x y =
              showParen (p > pr) $
                showsPrec pr x . (s ++) .
                showParen (prec y == pr) (showsPrec pr y) -- same as `showsPrec (pr+1) y`

-- | run `show` on a bunch `L`.
testL :: IO ()
testL = mapM_ putStrLn computations
  where computations :: [String]
        computations = [
            "1. C 1 ::+:: C 2 ::*:: C 3 ::+:: C 4",
            "= " <> show (C 1 ::+:: C 2 ::*:: C 3 ::+:: C 4),
            "2. (C 1 ::+:: C 2) ::*:: (C 3 ::+:: C 4)",
            "= " <> show ((C 1 ::+:: C 2) ::*:: (C 3 ::+:: C 4)),
            "3. C 1 ::+:: C 2 ::-:: C 3 ::-:: C 4",
            "= " <> show (C 1 ::+:: C 2 ::-:: C 3 ::-:: C 4),
            "4. C 1 ::+:: C 2 ::-:: (C 3 ::-:: C 4)",
            "= " <> show (C 1 ::+:: C 2 ::-:: (C 3 ::-:: C 4)),
            "5. (C 1 ::*:: (C 2 ::-:: (C 3 ::*:: (C 4 ::/:: C 5))))",
            "= " <> show (C 1 ::*:: (C 2 ::-:: (C 3 ::*:: (C 4 ::/:: C 5)))),
            "6. [C 1, C 2 ::*:: (C 3 ::+:: C 4), C 5 ::*:: C 6]",
            "= " <> show ([C 1, C 2 ::*:: (C 3 ::+:: C 4), C 5 ::*:: C 6]),
            "7. (C 1 ::+:: C 2, C 3 ::-:: (C 4 ::*:: C 5))",
            "= " <> show ((C 1 ::+:: C 2, C 3 ::-:: (C 4 ::*:: C 5)))
          ]

--------------------------------------------------------------------------------
-- | example 3: an efficient way to show a `Tree`.
-- source: /u/ Sven-Olof Nyström @ https://tinyurl.com/jmt7thd5 (uppsala univ)

-- | `Tree1` type.
data Tree1 a = Leaf1 a | Branch1 (Tree1 a) (Tree1 a)

-- | shows a `Tree1`.
showsTree              :: (Show a) => Tree1 a -> ShowS
showsTree (Leaf1 x)     =  shows x
showsTree (Branch1 l r) =
    ('<':) . showsTree l . ('|':) . showsTree r . ('>':)

-- | `Show` instance for `Tree1`.
instance Show a => Show (Tree1 a) where
   show t= showsTree t ""

-- | run show on a `Tree1`.
-- expected output: <"foo"|<"glob"|"hog">>
testTree1 :: IO()
testTree1 = print $ Branch1 (Leaf1 "foo") (Branch1 (Leaf1 "glob") (Leaf1 "hog"))

--------------------------------------------------------------------------------
-- | example 4: `Show` instance for type `TT`.
-- source: haskell 2010 report, chapter 11, @ https://tinyurl.com/2p833dbh
-- author: Prem Muthedath

-- | `TT` type.
infixr 4 :$
data TT = Int :$ TT  |  NT deriving Eq

-- | `Show` instance.
instance Show TT where
  showsPrec p e0 = case e0 of
    x :$ y -> showChild 4 " :$ " x y
    NT     -> showString "NT"
    where showChild :: Int -> String -> Int -> TT -> ShowS
          showChild pr s x y =
            showParen (p == pr) $
              showsPrec 11 x . (s ++) .
              showsPrec pr y

-- | run `show` on a bunch of `TT`.
-- NOTE: show (1 :$ 2 :$ NT) should produce "1 :$ (2 :$ NT)"
testTT :: IO ()
testTT = mapM_ putStrLn computations
  where computations :: [String]
        computations = [
            "1. 5 :$ 6 :$ NT",
            "= " <> show (5 :$ 6 :$ NT),
            "2. 7 :$ 8 :$ 11 :$ NT",
            "= " <> show (7 :$ 8 :$ 11 :$ NT),
            "3. 21 :$ NT",
            "= " <> show (21 :$ NT),
            "4. NT",
            "= " <> show (NT),
            "5. [NT, 1 :$ NT, 81 :$ 14 :$ NT]",
            "= " <> show ([NT, 1 :$ NT, 81 :$ 14 :$ NT]),
            "6. (NT, 81 :$ 14 :$ NT)",
            "= " <> show ((NT, 81 :$ 14 :$ NT))
          ]

--------------------------------------------------------------------------------
