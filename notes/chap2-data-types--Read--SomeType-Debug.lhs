-- | GHCi debug  of `read "(3 4)" :: SomeType`.
-- source code @ ../src/C2Read.hs
-- author: Prem Muthedath, NOV 2021.

--------------------------------------------------------------------------------
-- | top-level `readsPrec` called from `read`.
*Main λ ▶▶ :step read "(3 4)" :: SomeType Int
Stopped in Main.readsPrec, temp.hs:57:19-41
_result :: [(SomeType a, String)] = _
r :: String = _
readMix :: String -> [(SomeType a, String)] = _
readType :: String -> [(SomeType a, String)] = _
[temp.hs:57:19-41]
*Main λ ▶▶ :list
56  instance (Read a) => Read (SomeType a) where
57    readsPrec d r = readMix r ++ readType r
58        where readMix :: String -> [(SomeType a, String)]
[temp.hs:57:19-41]

-- | top-level `readsPrec` call.
*Main λ ▶▶ :step
Stopped in Main.readsPrec, temp.hs:57:19-27
_result :: [(SomeType a, String)] = _
r :: String = _
readMix :: String -> [(SomeType a, String)] = _
[temp.hs:57:19-27]
*Main λ ▶▶ :list
56  instance (Read a) => Read (SomeType a) where
57    readsPrec d r = readMix r ++ readType r
58        where readMix :: String -> [(SomeType a, String)]
[temp.hs:57:19-27]

-- | `readMix` code, as part of call from top-level `readsPrec`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:(59,23)-(62,36)
_result :: ReadS (SomeType a) = _
d :: Int = 0
[temp.hs:(59,23)-(62,36)]
*Main λ ▶▶ :list
58        where readMix :: String -> [(SomeType a, String)]
59              readMix = readParen' True $ \r -> do
60                (v1, r'') :: (SomeType a, String) <- readsPrec d r
61                (v2, r')  :: (SomeType a, String) <- readsPrec d r''
62                return (Mix v1 v2, r')
63              readType   :: String -> [(SomeType a, String)]
[temp.hs:(59,23)-(62,36)]

-- | `readMix` called from top-level `readsPrec`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:59:23-37
_result :: ReadS (SomeType a) -> ReadS (SomeType a) = _
[temp.hs:59:23-37]
*Main λ ▶▶ :list
58        where readMix :: String -> [(SomeType a, String)]
59              readMix = readParen' True $ \r -> do
60                (v1, r'') :: (SomeType a, String) <- readsPrec d r
[temp.hs:59:23-37]

-- | `readParen` in `readMix` called from top-level `readsPrec`.
*Main λ ▶▶ :step
Stopped in Main.readParen', temp.hs:35:20-52
_result :: ReadS a = _
b :: Bool = True
mandatory :: ReadS a = _
optional :: ReadS a = _
[temp.hs:35:20-52]
*Main λ ▶▶ :list
34  readParen'      :: forall a. Bool -> ReadS a -> ReadS a
35  readParen' b g  =  if b then mandatory else optional
36                     where optional    :: ReadS a
[temp.hs:35:20-52]

-- | `mandatory` in `readParen` in `readMix` called from top-level `readsPrec`.
*Main λ ▶▶ :step
Stopped in Main.readParen'.mandatory, temp.hs:(39,40)-(41,86)
_result :: [(a, String)] = _
optional :: ReadS a = _
r :: String = _
[temp.hs:(39,40)-(41,86)]
*Main λ ▶▶ :list
38                           mandatory   :: ReadS a
39                           mandatory r = [(x,u) | ("(",s) :: (String, String) <- lex r,
40                                                  (x,t)   :: (a, String)      <- optional s,
41                                                  (")",u) :: (String, String) <- lex t ]
42
[temp.hs:(39,40)-(41,86)

-- | `lex` parsing "(" in "(3 4)" in `mandatory` in `readParen` in `readMix` in 
-- top-level `readsPrec` call. after this parse, we're left with "3 4)".
*Main λ ▶▶ :step
Stopped in Main.readParen'.mandatory, temp.hs:39:80-84
_result :: [(String, String)] = _
r :: String = _
[temp.hs:39:80-84]
*Main λ ▶▶ :list
38                           mandatory   :: ReadS a
39                           mandatory r = [(x,u) | ("(",s) :: (String, String) <- lex r,
40                                                  (x,t)   :: (a, String)      <- optional s,
[temp.hs:39:80-84]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | parsing "3, 4)" using `optional s`, where s = "3, 4)"; `optional` called 
-- from `mandatory` in `readParen` in `readMix` in top-level `readsParen`.
*Main λ ▶▶ :step
Stopped in Main.readParen'.mandatory, temp.hs:40:80-89
_result :: [(a, String)] = _
optional :: ReadS a = _
s :: String = _
[temp.hs:40:80-89]
*Main λ ▶▶ :list
39                           mandatory r = [(x,u) | ("(",s) :: (String, String) <- lex r,
40                                                  (x,t)   :: (a, String)      <- optional s,
41                                                  (")",u) :: (String, String) <- lex t ]
[temp.hs:40:80-89]

-- | looking at code for `optional`, called from `mandatory` in `readParen` in 
-- `readMix` in top-level `readsPrec`.
*Main λ ▶▶ :step
Stopped in Main.readParen'.optional, temp.hs:37:40-57
_result :: [(a, String)] = _
g :: ReadS a = _
mandatory :: ReadS a = _
r :: String = _
[temp.hs:37:40-57]
*Main λ ▶▶ :list
36                     where optional    :: ReadS a
37                           optional r  = g r ++ mandatory r
38                           mandatory   :: ReadS a
[temp.hs:37:40-57]

--------------------------------------------------------------------------------
-- | `g r` computation -- part of `optional` in `mandatory` in `readParen` in 
-- `readMix` in top-level `readsPrec`.
*Main λ ▶▶ :step
Stopped in Main.readParen'.optional, temp.hs:37:40-42
_result :: [(a, String)] = _
g :: ReadS a = _
r :: String = _
[temp.hs:37:40-42]
*Main λ ▶▶ :list
36                     where optional    :: ReadS a
37                           optional r  = g r ++ mandatory r
38                           mandatory   :: ReadS a
[temp.hs:37:40-42]

-- | looking at entire lambda expression for `g`, as part `optional` call.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:(59,47)-(62,36)
_result :: [(SomeType a, String)] = _
d :: Int = 0
r :: String = _
[temp.hs:(59,47)-(62,36)]
*Main λ ▶▶ :list
58        where readMix :: String -> [(SomeType a, String)]
59              readMix = readParen' True $ \r -> do
60                (v1, r'') :: (SomeType a, String) <- readsPrec d r
61                (v2, r')  :: (SomeType a, String) <- readsPrec d r''
62                return (Mix v1 v2, r')
63              readType   :: String -> [(SomeType a, String)]
[temp.hs:(59,47)-(62,36)]

--------------------------------------------------------------------------------
-- computing 1st line in `g`, the lambda expression, called from `optional` in 
-- `mandatory` in `readParen` in `readMix` in top-level `readsPrec`.
-- this first line is 1st `readsPrec` call in `g`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:60:52-64
_result :: [(SomeType a, String)] = _
d :: Int = 0
r :: String = _
[temp.hs:60:52-64]
*Main λ ▶▶ :list
59              readMix = readParen' True $ \r -> do
60                (v1, r'') :: (SomeType a, String) <- readsPrec d r
61                (v2, r')  :: (SomeType a, String) <- readsPrec d r''
[temp.hs:60:52-64]

-- | 1st `readsPrec` call from `g r` in `optional` in `mandatory` in `readParen` 
-- in `readMix` in top-level `readsPrec` call; looking at `readsPrec` code.
*Main λ ▶▶ :step
Stopped in Main.readsPrec, temp.hs:57:19-41
_result :: [(SomeType a, String)] = _
r :: String = _
readMix :: String -> [(SomeType a, String)] = _
readType :: String -> [(SomeType a, String)] = _
[temp.hs:57:19-41]
*Main λ ▶▶ :list
56  instance (Read a) => Read (SomeType a) where
57    readsPrec d r = readMix r ++ readType r
58        where readMix :: String -> [(SomeType a, String)]
[temp.hs:57:19-41]

-- | looking at `readMix`, as part of 1st `readsPrec` call in `optional` `g r`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec, temp.hs:57:19-27
_result :: [(SomeType a, String)] = _
r :: String = _
readMix :: String -> [(SomeType a, String)] = _
[temp.hs:57:19-27]
*Main λ ▶▶ :list
56  instance (Read a) => Read (SomeType a) where
57    readsPrec d r = readMix r ++ readType r
58        where readMix :: String -> [(SomeType a, String)]
[temp.hs:57:19-27]

-- | looking at `readMix` code called from 1st `readsPrec` in `optional` `g r`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:(59,23)-(62,36)
_result :: ReadS (SomeType a) = _
d :: Int = 0
[temp.hs:(59,23)-(62,36)]
*Main λ ▶▶ :list
58        where readMix :: String -> [(SomeType a, String)]
59              readMix = readParen' True $ \r -> do
60                (v1, r'') :: (SomeType a, String) <- readsPrec d r
61                (v2, r')  :: (SomeType a, String) <- readsPrec d r''
62                return (Mix v1 v2, r')
63              readType   :: String -> [(SomeType a, String)]
[temp.hs:(59,23)-(62,36)]

-- | `readParen'` call in `readMix` in 1st `readsPrec` in `optional` `g r` in 
-- `mandatory` in `readParen` in `readMix` in top-level `readsPrec`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:59:23-37
_result :: ReadS (SomeType a) -> ReadS (SomeType a) = _
[temp.hs:59:23-37]
*Main λ ▶▶ :list
58        where readMix :: String -> [(SomeType a, String)]
59              readMix = readParen' True $ \r -> do
60                (v1, r'') :: (SomeType a, String) <- readsPrec d r
[temp.hs:59:23-37]

-- | `readParen` called from `readMix` in 1st `readsPrec` in `optional` `g r` in 
-- `mandatory` in `readParen` in `readMix` in top-level `readsPrec`.
*Main λ ▶▶ :step
Stopped in Main.readParen', temp.hs:35:20-52
_result :: ReadS a = _
b :: Bool = True
mandatory :: ReadS a = _
optional :: ReadS a = _
[temp.hs:35:20-52]
*Main λ ▶▶ :list
34  readParen'      :: forall a. Bool -> ReadS a -> ReadS a
35  readParen' b g  =  if b then mandatory else optional
36                     where optional    :: ReadS a
[temp.hs:35:20-52]

-- | looking at `mandatory` code -- called from `readParen' in `readMix` in 1st 
-- `readsPrec` in `optional` `g r` in `mandatory` in `readParen` in `readMix`.
*Main λ ▶▶ :step
Stopped in Main.readParen'.mandatory, temp.hs:(39,40)-(41,86)
_result :: [(a, String)] = _
optional :: ReadS a = _
r :: String = _
[temp.hs:(39,40)-(41,86)]
*Main λ ▶▶ :list
38                           mandatory   :: ReadS a
39                           mandatory r = [(x,u) | ("(",s) :: (String, String) <- lex r,
40                                                  (x,t)   :: (a, String)      <- optional s,
41                                                  (")",u) :: (String, String) <- lex t ]
42
[temp.hs:(39,40)-(41,86)]

-- | computing 1st line -- `lex` call -- in `mandatory` code.
-- tries to parse "(" in "3 4)" and fails, so exits `mandatory` altogether.
-- this ends `readParen` call from `readMix`. since `readParen` has ended, so  
-- has `readMix` call from 1st `readsPrec` in `optional` `g r`.
*Main λ ▶▶ :step
Stopped in Main.readParen'.mandatory, temp.hs:39:80-84
_result :: [(String, String)] = _
r :: String = _
[temp.hs:39:80-84]
*Main λ ▶▶ :list
38                           mandatory   :: ReadS a
39                           mandatory r = [(x,u) | ("(",s) :: (String, String) <- lex r,
40                                                  (x,t)   :: (a, String)      <- optional s,
[temp.hs:39:80-84]

-- | since `readMix` call has returned, `readsPrec` next calls `readType`.  
-- `readsPrec` here is the 1st one in `optional` `g r`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec, temp.hs:57:32-41
_result :: [(SomeType a, String)] = _
r :: String = '3' : ' ' : _
readType :: String -> [(SomeType a, String)] = _
[temp.hs:57:32-41]
*Main λ ▶▶ :list
56  instance (Read a) => Read (SomeType a) where
57    readsPrec d r = readMix r ++ readType r
58        where readMix :: String -> [(SomeType a, String)]
[temp.hs:57:32-41]

-- | code in `readType`, called from 1st `readsPrec` in `optional` `g r`.
-- NOTE: `readsPrec` in `readType` is one defined for `Int`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readType, temp.hs:(64,26)-(66,33)
_result :: [(SomeType a, String)] = _
d :: Int = 0
r :: String = '3' : ' ' : _
[temp.hs:(64,26)-(66,33)]
*Main λ ▶▶ :list
63              readType   :: String -> [(SomeType a, String)]
64              readType r = do
65                (v, r') :: (a, String) <- readsPrec d r
66                return (Type v, r')
67
[temp.hs:(64,26)-(66,33)]

-- | calling `readsPrec` (for `Int`) in `readType` from 1st `readsPrec` in 
-- `optional` `g r`. `r` = "3 4)", so parses to `(3, " 4)")`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readType, temp.hs:65:41-53
_result :: [(a, String)] = _
d :: Int = 0
r :: String = '3' : ' ' : _
[temp.hs:65:41-53]
*Main λ ▶▶ :list
64              readType r = do
65                (v, r') :: (a, String) <- readsPrec d r
66                return (Type v, r')
[temp.hs:65:41-53]

-- | returning `(Type 3, " 4)")` from `readType`.
-- this is result of 1st `readsPrec` call from `g r`, called from `optional`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readType, temp.hs:66:15-33
_result :: [(SomeType a, String)] = _
r' :: String = ' ' : _
v :: a = _
[temp.hs:66:15-33]
*Main λ ▶▶ :list
65                (v, r') :: (a, String) <- readsPrec d r
66                return (Type v, r')
67
[temp.hs:66:15-33]

--------------------------------------------------------------------------------
-- | 2nd `readsPrec` call in `g r` -- called from `optional` in `mandatory` in 
-- `readParen` in `readMix` in top-level `readsPrec`.
-- `r''` is " 4)"
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:61:52-66
_result :: [(SomeType a, String)] = _
d :: Int = 0
r'' :: String = ' ' : _
[temp.hs:61:52-66]
*Main λ ▶▶ :list
60                (v1, r'') :: (SomeType a, String) <- readsPrec d r
61                (v2, r')  :: (SomeType a, String) <- readsPrec d r''
62                return (Mix v1 v2, r')
[temp.hs:61:52-66]

-- | `readsPrec` code (2nd), part of `g r` called from `optional` (i.e.  `g r`).
*Main λ ▶▶ :step
Stopped in Main.readsPrec, temp.hs:57:19-41
_result :: [(SomeType a, String)] = _
r :: String = ' ' : _
readMix :: String -> [(SomeType a, String)] = _
readType :: String -> [(SomeType a, String)] = _
[temp.hs:57:19-41]
*Main λ ▶▶ :list
56  instance (Read a) => Read (SomeType a) where
57    readsPrec d r = readMix r ++ readType r
58        where readMix :: String -> [(SomeType a, String)]
[temp.hs:57:19-41]

-- | `readMix` part in 2nd `readsPrec` call in `optional` (i.e. `g r`).
*Main λ ▶▶ :step
Stopped in Main.readsPrec, temp.hs:57:19-27
_result :: [(SomeType a, String)] = _
r :: String = ' ' : _
readMix :: String -> [(SomeType a, String)] = _
[temp.hs:57:19-27]
*Main λ ▶▶ :list
56  instance (Read a) => Read (SomeType a) where
57    readsPrec d r = readMix r ++ readType r
58        where readMix :: String -> [(SomeType a, String)]
[temp.hs:57:19-27]

-- | `readMix` code -- part of 2nd `readsPrec` call in `optional` (i.e., `g r`).
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:(59,23)-(62,36)
_result :: ReadS (SomeType a) = _
d :: Int = 0
[temp.hs:(59,23)-(62,36)]
*Main λ ▶▶ :list
58        where readMix :: String -> [(SomeType a, String)]
59              readMix = readParen' True $ \r -> do
60                (v1, r'') :: (SomeType a, String) <- readsPrec d r
61                (v2, r')  :: (SomeType a, String) <- readsPrec d r''
62                return (Mix v1 v2, r')
63              readType   :: String -> [(SomeType a, String)]
[temp.hs:(59,23)-(62,36)]

-- | `readParen` call from `readMix` in 2nd `readsPrec` in `optional` `g r`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:59:23-37
_result :: ReadS (SomeType a) -> ReadS (SomeType a) = _
[temp.hs:59:23-37]
*Main λ ▶▶ :list
58        where readMix :: String -> [(SomeType a, String)]
59              readMix = readParen' True $ \r -> do
60                (v1, r'') :: (SomeType a, String) <- readsPrec d r
[temp.hs:59:23-37]

-- | `readParen`: called from `readMix` in 2nd `readsPrec` in `optional` `g r`.
*Main λ ▶▶ :step
Stopped in Main.readParen', temp.hs:35:20-52
_result :: ReadS a = _
b :: Bool = True
mandatory :: ReadS a = _
optional :: ReadS a = _
[temp.hs:35:20-52]
*Main λ ▶▶ :list
34  readParen'      :: forall a. Bool -> ReadS a -> ReadS a
35  readParen' b g  =  if b then mandatory else optional
36                     where optional    :: ReadS a
[temp.hs:35:20-52]

-- | `mandatory` code -- called from `readParen` in `readMix` in 2nd `readsPrec` 
-- in `optional` `g r`. `r`, the string, is " 4)".
*Main λ ▶▶ :step
Stopped in Main.readParen'.mandatory, temp.hs:(39,40)-(41,86)
_result :: [(a, String)] = _
optional :: ReadS a = _
r :: String = ' ' : _
[temp.hs:(39,40)-(41,86)]
*Main λ ▶▶ :list
38                           mandatory   :: ReadS a
39                           mandatory r = [(x,u) | ("(",s) :: (String, String) <- lex r,
40                                                  (x,t)   :: (a, String)      <- optional s,
41                                                  (")",u) :: (String, String) <- lex t ]
42
[temp.hs:(39,40)-(41,86)]

-- | `lex` call in `mandatory` tries to parse "(" from " 4)" and fails.
-- so `mandatory` returns to `readParen`, which returns to `readMix`, which 
-- returns to 2nd `readsPrec` in `optional` `g r`.
*Main λ ▶▶ :step
Stopped in Main.readParen'.mandatory, temp.hs:39:80-84
_result :: [(String, String)] = _
r :: String = ' ' : _
[temp.hs:39:80-84]
*Main λ ▶▶ :list
38                           mandatory   :: ReadS a
39                           mandatory r = [(x,u) | ("(",s) :: (String, String) <- lex r,
40                                                  (x,t)   :: (a, String)      <- optional s,
[temp.hs:39:80-84]

-- | `readType` call from 2nd `readsPrec` in `optional` `g r`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec, temp.hs:57:32-41
_result :: [(SomeType a, String)] = _
r :: String = ' ' : '4' : ')' : _
readType :: String -> [(SomeType a, String)] = _
[temp.hs:57:32-41]
*Main λ ▶▶ :list
56  instance (Read a) => Read (SomeType a) where
57    readsPrec d r = readMix r ++ readType r
58        where readMix :: String -> [(SomeType a, String)]
[temp.hs:57:32-41]

-- | `readType` code called from 2nd `readsPrec` in `optional` `g r`.
-- NOTE: `readsPrec` in `readType` is for `Int`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readType, temp.hs:(64,26)-(66,33)
_result :: [(SomeType a, String)] = _
d :: Int = 0
r :: String = ' ' : '4' : ')' : _
[temp.hs:(64,26)-(66,33)]
*Main λ ▶▶ :list
63              readType   :: String -> [(SomeType a, String)]
64              readType r = do
65                (v, r') :: (a, String) <- readsPrec d r
66                return (Type v, r')
67
[temp.hs:(64,26)-(66,33)]

-- | `readsPrec` in `readType` called from 2nd `readsPrec` in `optional` `g r`.
-- `r` is " 4)", so `readsPrec` 0 `r` returns `(4, ")").
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readType, temp.hs:65:41-53
_result :: [(a, String)] = _
d :: Int = 0
r :: String = ' ' : '4' : ')' : _
[temp.hs:65:41-53]
*Main λ ▶▶ :list
64              readType r = do
65                (v, r') :: (a, String) <- readsPrec d r
66                return (Type v, r')
[temp.hs:65:41-53]

-- | `readType` call from 2nd `readsPrec` in `optional` `g r`.
-- returns `(Type 4, ")")`.  completes 2nd `readsPrec` call in `optional` `g r`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readType, temp.hs:66:15-33
_result :: [(SomeType a, String)] = _
r' :: String = ')' : _
v :: a = _
[temp.hs:66:15-33]
*Main λ ▶▶ :list
65                (v, r') :: (a, String) <- readsPrec d r
66                return (Type v, r')
67
[temp.hs:66:15-33]

--------------------------------------------------------------------------------
-- | 1st & 2nd `readsPrec` calls in `optional` `g r` is now complete;
-- final statement in `optional` `g r` -- `return (Mix v1, v2, r')`.
-- this returns `return ((Mix 3, 4), ")").
-- this completes `optional` `g r` in `mandatory` in `readParen` in `readMix`.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:62:15-36
_result :: [(SomeType a, String)] = _
r' :: String = ')' : _
v1 :: SomeType a = _
v2 :: SomeType a = _
[temp.hs:62:15-36]
*Main λ ▶▶ :list
61                (v2, r')  :: (SomeType a, String) <- readsPrec d r''
62                return (Mix v1 v2, r')
63              readType   :: String -> [(SomeType a, String)]
[temp.hs:62:15-36]

--------------------------------------------------------------------------------
-- | `mandatory` in `readParen` in `readMix`.
-- `optional` `g r` is now done, but `optional` has 1 more part: `mandatory r`.
-- for some reason, haskell looks at this next (see below), but for now it 
-- computes last `lex` of (")", u) in `mandatory` in `readParen` in `readMix`.
-- after `optional` `g r`, we have t = ")", so `lex t` gives ("(", ""), and this 
-- ends `mandatory` in `readParen` in `readMix` EXCEPT for `mandatory r` 
-- remaining in `optional` called from `mandatory` in `readParen` in `readMix`.
*Main λ ▶▶ :step
Stopped in Main.readParen'.mandatory, temp.hs:41:80-84
_result :: [(String, String)] = _
t :: String = ')' : _
[temp.hs:41:80-84]
*Main λ ▶▶ :list
40                                                  (x,t)   :: (a, String)      <- optional s,
41                                                  (")",u) :: (String, String) <- lex t ]
42
[temp.hs:41:80-84]

-- | `mandatory r` in `optional`, called from `mandatory` in `readParen`.
*Main λ ▶▶ :step
Stopped in Main.readParen'.optional, temp.hs:37:47-57
_result :: [(a, String)] = _
mandatory :: ReadS a = _
r :: String = "3 4)"
[temp.hs:37:47-57]
*Main λ ▶▶ :list
36                     where optional    :: ReadS a
37                           optional r  = g r ++ mandatory r
38                           mandatory   :: ReadS a
[temp.hs:37:47-57]

-- | `mandatory r` code called from `optional` in `mandatory` in `readParen`.
*Main λ ▶▶ :step
Stopped in Main.readParen'.mandatory, temp.hs:(39,40)-(41,86)
_result :: [(a, String)] = _
optional :: ReadS a = _
r :: String = "3 4)"
[temp.hs:(39,40)-(41,86)]
*Main λ ▶▶ :list
38                           mandatory   :: ReadS a
39                           mandatory r = [(x,u) | ("(",s) :: (String, String) <- lex r,
40                                                  (x,t)   :: (a, String)      <- optional s,
41                                                  (")",u) :: (String, String) <- lex t ]
42
[temp.hs:(39,40)-(41,86)]

-- | `mandatory r` in `optional` in `mandatory` in `readParen` in `readMix`.  
-- `r` is "3 4)", and we're parsing this for `("(", s)` in `lex`, which fails.
-- so this whole code returns (because of `do`), & so does `optional` called 
-- from `mandatory` in `readParen` in `readMix`.  the final value of this 
-- `optional` call from `mandatory` in `readParen` is `(Mix 3 4, ")")`.

-- NOTE: AT THIS POINT, WE'RE DONE WITH `mandatory` in `readParen` in `readMix` 
-- because of the previous out-of-order step (see above).

-- WE'RE ALSO DONE WITH `readParen` in `readMix`, AS WELL AS WITH `readMix` AT 
-- THIS POINT. BUT AGAIN, HASKELL DELAYS THE RETURN VALUE FROM `readMix` later 
-- (see below), THOUGH FOR ALL PURPOSES `readMix` IS NOW COMPLETE.
*Main λ ▶▶ :step
Stopped in Main.readParen'.mandatory, temp.hs:39:80-84
_result :: [(String, String)] = _
r :: String = "3 4)"
[temp.hs:39:80-84]
*Main λ ▶▶ :list
38                           mandatory   :: ReadS a
39                           mandatory r = [(x,u) | ("(",s) :: (String, String) <- lex r,
40                                                  (x,t)   :: (a, String)      <- optional s,
[temp.hs:39:80-84]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | since we're DONE with `readMix` (practically!), we next look at `readType` 
-- that is part of `readsPrec`, the top-level `readsPrec` call.
*Main λ ▶▶ :step
Stopped in Main.readsPrec, temp.hs:57:32-41
_result :: [(SomeType a, String)] = _
r :: String = "(3 4)"
readType :: String -> [(SomeType a, String)] = _
[temp.hs:57:32-41]
*Main λ ▶▶ :list
56  instance (Read a) => Read (SomeType a) where
57    readsPrec d r = readMix r ++ readType r
58        where readMix :: String -> [(SomeType a, String)]
[temp.hs:57:32-41]

-- | `readType` in `readsPrec`, the top-level call.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readType, temp.hs:(64,26)-(66,33)
_result :: [(SomeType a, String)] = _
d :: Int = 0
r :: String = "(3 4)"
[temp.hs:(64,26)-(66,33)]
*Main λ ▶▶ :list
63              readType   :: String -> [(SomeType a, String)]
64              readType r = do
65                (v, r') :: (a, String) <- readsPrec d r
66                return (Type v, r')
67
[temp.hs:(64,26)-(66,33)]

-- | `readsPrec` (this is NOT for `SomeType`) call in `readType` in top-level 
-- `readsPrec`.  `r` is "(3 4)", and we need `Type v`, so this parse fails, but 
-- because of the `do` block, the error is handled automatically.

-- `readType` CALL IS COMPLETE, & SO IS THE SO IS THE TOP-LEVEL `readsPrec` 
-- EXCEPT FOR THE WHACKY ORDER OF HASKELL EXECUTION (SEE BELOW).
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readType, temp.hs:65:41-53
_result :: [(a, String)] = _
d :: Int = 0
r :: String = "(3 4)"
[temp.hs:65:41-53]
*Main λ ▶▶ :list
64              readType r = do
65                (v, r') :: (a, String) <- readsPrec d r
66                return (Type v, r')
[temp.hs:65:41-53]

--------------------------------------------------------------------------------
-- | `readMix` `return`, a result of top-level `readsPrec` call.
-- as stated before, this call should have been handled before, rather than 
-- here, but for some reason haskell does it in this order (laziness?).

-- `readMix` is now complete and it returns `return ((Mix 3 4), "").
-- ALSO, AT THIS POINT, TOP-LEVEL `readsPrec` IS ALSO COMPLETE.
*Main λ ▶▶ :step
Stopped in Main.readsPrec.readMix, temp.hs:62:23-31
_result :: SomeType a = _
v1 :: SomeType a = _
v2 :: SomeType a = _
[temp.hs:62:23-31]
*Main λ ▶▶ :list
61                (v2, r')  :: (SomeType a, String) <- readsPrec d r''
62                return (Mix v1 v2, r')
63              readType   :: String -> [(SomeType a, String)]
[temp.hs:62:23-31]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
*Main λ ▶▶ :step
Stopped in Main.show, temp.hs:54:20-56
_result :: [Char] = _
a :: SomeType a = _
b :: SomeType a = _
[temp.hs:54:20-56]
*Main λ ▶▶ :list
53    show (Type a) = show a
54    show (Mix a b) = "(" ++ show a ++ " " ++ show b ++ ")"
55
[temp.hs:54:20-56]

*Main λ ▶▶ :step
(Stopped in Main.show, temp.hs:54:27-56
_result :: [Char] = _
a :: SomeType a = _
b :: SomeType a = _
[temp.hs:54:27-56]
*Main λ ▶▶ :list
53    show (Type a) = show a
54    show (Mix a b) = "(" ++ show a ++ " " ++ show b ++ ")"
55
[temp.hs:54:27-56]

*Main λ ▶▶ :step
Stopped in Main.show, temp.hs:54:27-32
_result :: String = _
a :: SomeType a = _
[temp.hs:54:27-32]
*Main λ ▶▶ :list
53    show (Type a) = show a
54    show (Mix a b) = "(" ++ show a ++ " " ++ show b ++ ")"
55
[temp.hs:54:27-32]

*Main λ ▶▶ :step
Stopped in Main.readsPrec.readType, temp.hs:66:23-28
_result :: SomeType a = _
v :: a = _
[temp.hs:66:23-28]
*Main λ ▶▶ :list
65                (v, r') :: (a, String) <- readsPrec d r
66                return (Type v, r')
67
[temp.hs:66:23-28]

*Main λ ▶▶ :step
Stopped in Main.show, temp.hs:53:19-24
_result :: String = _
a :: a = _
[temp.hs:53:19-24]
*Main λ ▶▶ :list
52  instance (Show a) => Show (SomeType a) where
53    show (Type a) = show a
54    show (Mix a b) = "(" ++ show a ++ " " ++ show b ++ ")"
[temp.hs:53:19-24]

*Main λ ▶▶ :step
3Stopped in Main.show, temp.hs:54:37-56
_result :: [Char] = _
b :: SomeType a = _
[temp.hs:54:37-56]
*Main λ ▶▶ :list
53    show (Type a) = show a
54    show (Mix a b) = "(" ++ show a ++ " " ++ show b ++ ")"
55
[temp.hs:54:37-56]

*Main λ ▶▶ :step
 Stopped in Main.show, temp.hs:54:44-56
_result :: [Char] = _
b :: SomeType a = _
[temp.hs:54:44-56]
*Main λ ▶▶ :list
53    show (Type a) = show a
54    show (Mix a b) = "(" ++ show a ++ " " ++ show b ++ ")"
55
[temp.hs:54:44-56]

*Main λ ▶▶ :step
Stopped in Main.show, temp.hs:54:44-49
_result :: String = _
b :: SomeType a = _
[temp.hs:54:44-49]
*Main λ ▶▶ :list
53    show (Type a) = show a
54    show (Mix a b) = "(" ++ show a ++ " " ++ show b ++ ")"
55
[temp.hs:54:44-49]

*Main λ ▶▶ :step
Stopped in Main.readsPrec.readType, temp.hs:66:23-28
_result :: SomeType a = _
v :: a = _
[temp.hs:66:23-28]
*Main λ ▶▶ :list
65                (v, r') :: (a, String) <- readsPrec d r
66                return (Type v, r')
67
[temp.hs:66:23-28]

*Main λ ▶▶ :step
Stopped in Main.show, temp.hs:53:19-24
_result :: String = _
a :: a = _
[temp.hs:53:19-24]
*Main λ ▶▶ :list
52  instance (Show a) => Show (SomeType a) where
53    show (Type a) = show a
54    show (Mix a b) = "(" ++ show a ++ " " ++ show b ++ ")"
[temp.hs:53:19-24]

*Main λ ▶▶ :step
4)

*Main λ ▶▶ :list
Not stopped at a breakpoint; nothing to list

*Main λ ▶▶

--------------------------------------------------------------------------------
