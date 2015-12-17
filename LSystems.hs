module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush, island, carpet, rectangles, cristian :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (x, y, z)
 = x

-- |Returns the base string for the given system.
base :: System -> String
base (x, y, z)
 = y

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (x, y, z)
 = z


-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar ch rs
 = concat [b | (a,b) <- rs, a == ch]

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne rs torpl
 = concat (map (`lookupChar` rs) torpl)

-- | Alternative of expandOne using list comprehension
expandOne2 :: Rules -> String -> String
expandOne2 rs torpl
 = concat [lookupChar x rs | x <- torpl]

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand rs torpl 0 = torpl
expand rs torpl n
 = (iterate (expandOne rs) torpl) !! n

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move 'L' ((x,y),z) t = ((x,y),(z+t))
move 'R' ((x,y),z) t = ((x,y),(z-t))
move 'F' ((x,y),z) t = ( (x+cos th, y + sin th), z)
 where
  th = z * pi / 180

-- |Trace lines drawn by a turtle using the given colour, following the
trace :: String -> Float -> Colour -> [ColouredLine]
trace str angl col
 = reverse (trace' str ((0,0), 90) [((0,0), 90)])
  where
   trace' :: String -> TurtleState -> Stack -> [ColouredLine]
   trace' "" _ _ = []
   trace' (x:xs) state (s:st)
    | x == 'F' = ( q1, t1, col) : (trace' xs t (s:st))
    | x == '[' = trace' xs state (state : (s:st))
    | x == ']' = trace' xs s st
    | otherwise = trace' xs t (s:st)
     where
      t@(t1, t2) = move x state angl
      (q1, q2) = state


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

-- Additional examples

island
 = ( 90
   , "M+M+M+M"
   , [('M', "M+N-MM+M+MM+MN+MM-N+MM-M-MM-MN-MMM")
   , ('N', "NNNNNN")
   , ('[', "[")
   , (']', "]")
   , ('+', "+")
   , ('-', "-")
     ]
    )

carpet
 = ( 90
   , "M-M-M-M"
   , [('M', "MM-M-M-M-MM")
   , ('[', "[")
   , (']', "]")
   , ('+', "+")
   , ('-', "-")
     ]
   )

rectangles
 = ( 90
   , "M-M-M-M"
   , [('M', "MM-M+M-M-MM")
   , ('[', "[")
   , (']', "]")
   , ('+', "+")
   , ('-', "-")
     ]
   )

cristian
 = ( 10
   , "M"
   , [('M',"M[+M][-M][MM]")
   , ('[', "[")
   , (']', "]")
   , ('+', "+")
   , ('-', "-")
     ]
   )
mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

-- |Takes: system name, no of iterations, colour and draws the system picture
drawLSystem :: System -> Int -> Colour -> IO ()
drawLSystem system n colour
  = drawLines (trace (lSystem system n) (angle system) colour)

-- |Takes: system name, no of iterations, a list of colours and
--  draws the system picture using colours for each level
drawLSystem2 :: System -> Int -> [Colour] -> IO ()
drawLSystem2 system n colours
  = drawLines (trace2 (lSystem system n) (angle system) colours)


-- |Equivalent of trace but using a list of colours
--  All branches sharing a root have the same colour
--  Operates from outside towards inside
trace2 :: String -> Float -> [Colour] -> [ColouredLine]
trace2 str angl cols
 = reverse (tr' str ((0,0), 90) [((0,0), 90)])
  where
   tr' :: String -> TurtleState -> Stack -> [ColouredLine]
   tr' "" _ _ = []
   tr' (x:xs) state r@(s:st)
    | (x == 'F') && (nr == 0)     = (q1, t1, (cols !! 0)) : (tr' xs t r)
    | (x == 'F') && (nr <= nrcol) = (q1, t1, (cols !! (nr - 1))) : (tr' xs t r)
    | (x == 'F') && (nr >= nrcol) = (q1, t1, (cols !! nrc')) : (tr' xs t r)
    | x == '[' = tr' xs state (state : r)
    | x == ']' = tr' xs s st
    | otherwise = tr' xs t r
     where
      t@(t1, t2) = move x state angl
      (q1, q2) = state
      nr = length st
      nrcol = length cols
      nrc' = nrcol - 1

-- | Takes: system name, no of iterations, two colours and
--   draws the system picture as drawLSystem2 does,
--   using colours fading between the two given ones
--   Pre: Input system must have branches (n>1)
drawLSystem3 :: System -> Int -> (Colour,Colour) -> IO ()
drawLSystem3 system n (col1, col2)
  = drawLines (trace2 (lSystem system n) (angle system) (intpl col1 col2 nr))
   where
    nr = structlen (lSystem system n) (angle system)

-- | Takes: two colours; Returns: list of colours fading from 1st to 2nd
--   Interpolations by percentage
intpl :: Colour -> Colour -> Int -> [Colour]
intpl (x1, y1, z1) (x2, y2, z2) n
 = doclr n
 where
  doclr :: Int -> [Colour]
  doclr 0 = []
  doclr t
   = (a, b, c) : (doclr (t - 1))
   where
    k = fromIntegral t
    prc1 = k / (fromIntegral n)
    prc2 = 1 - prc1
    a = prc1 * x1 + prc2 * x2
    b = prc1 * y1 + prc2 * y2
    c = prc1 * z1 + prc2 * z2

-- | Computes the number of "roots" of a system
structlen :: String -> Float -> Int
structlen str angl
 = maximum (strl' str ((0,0), 90) [((0,0), 90)])
  where
   strl' :: String -> TurtleState -> Stack -> [Int]
   strl' "" _ _ = []
   strl' (x:xs) state q@(s:st)
    | x == 'F' = (strl' xs t q)
    | x == '[' = l : (strl' xs state (state : q))
    | x == ']' = strl' xs s st
    | otherwise = strl' xs t q
     where
      t = move x state angl
      l = length (s:st)
