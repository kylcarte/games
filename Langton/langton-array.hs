{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

import Data.List
import qualified Data.Map as M
import Data.Array as A
import Data.Ix

-- Types {{{

type Size = (Integer,Integer)
type ColorMap = M.Map Color (Direction -> Direction)

newtype X = X { unX :: Integer } deriving (Eq,Ord,Show,Enum,Num,Ix)
newtype Y = Y { unY :: Integer } deriving (Eq,Ord,Show,Enum,Num,Ix)
type Coord = (X,Y)

type Row a = Array X a
type Col a = Array Y a

type Grid a = Col (Row a)

instance Ord Color where
  compare c1 c2 = let
    (r1,g1,b1,a1) = rgbaOfColor c1
    (r2,g2,b2,a2) = rgbaOfColor c2 in
      let r = compare r1 r2 in case r of
        EQ -> let g = compare g1 g2 in case g of
          EQ -> let b = compare b1 b2 in case b of
            EQ -> compare a1 a2
            _ -> b
          _ -> g
        _ -> r

-- }}}

-- Grid Functions {{{

(#) :: Grid a -> Coord -> a
g # (x,y) = g ! y ! x

inRow :: X -> (a -> a) -> Row a -> Row a
inRow y f r = r // [(y,c')]
  where
  c = r ! y
  c' = f c

inGrid :: Coord -> (a -> a) -> Grid a -> Grid a
inGrid (x,y) f g = g // [(y,r')]
  where
  r = g ! y
  r' = inRow x f r

{-
subGrid :: Coord -> Size -> Grid a -> Grid a
subGrid (x,y) (sx,sy) = fmap (slice x sx) . slice y sy
-}

mapCoord :: (Coord -> a -> b) -> Grid a -> Grid b
mapCoord f = imap $ \y -> y `seq` imap $ \x -> x `seq` f (x,y)

solid :: Size -> Color -> Grid Color
solid (sx,sy) = repArray sy . repArray sx

repArray :: (Ix i, Enum i) => Integer -> a -> Array i a
repArray n x = array (zInd, nInd) $
  zip (enumFrom zInd) $ replicate (fromEnum n) x
  where
  zInd = toEnum 0
  nInd = toEnum $ fromEnum (n - 1)

blankGrid :: Size -> Grid Color
blankGrid size = solid size black

-- }}}

-- Direction {{{

data Direction = N | W | E | S deriving Show

left, right, straight, reverse :: Direction -> Direction
left d = case d of
  N -> W
  W -> S
  E -> N
  S -> E

right d = case d of
  N -> E
  W -> N
  E -> S
  S -> W

straight = id

reverse d = case d of
  N -> S
  W -> E
  E -> W
  S -> N

-- }}}

-- Rules {{{

data Rules = Rules
  { dirMap :: ColorMap
  , colors :: [Color]
  }

onDirMap :: (ColorMap -> ColorMap) -> Rules -> Rules
onDirMap f rs = rs { dirMap = f $ dirMap rs }

onColors :: ([Color] -> [Color]) -> Rules -> Rules
onColors f rs = rs { colors = f $ colors rs }

nextColor :: Rules -> Color -> Color
nextColor rs c = case dropWhile (/= c) cs of
  [_]    -> head cs
  _:c':_ -> c'
  []     -> error $ "Bad color for our rules: " ++ show c
  where
  cs = colors rs

-- }}}

-- GridState {{{

data GridState = GridState
  { grid      :: !(Grid Color)
  , antCoord  :: !Coord
  , antDir    :: !Direction
  , steps     :: !Integer
  , time      :: !Float
  , rules     :: !Rules
  }

instance Show GridState where
  show (GridState g c d _ _ _) = unlines
    [ show c
    , show d
    , show g
    ]

initGridState :: Size -> Rules -> GridState
initGridState size@(sx,sy) = GridState (blankGrid size) (x,y) E 0 0
  where
  x = X $ sx `div` 2
  y = Y $ sy `div` 2

onGrid :: (Grid Color -> Grid Color) -> GridState -> GridState
onGrid f gs = gs { grid = f $ grid gs }

onAntCoord :: (Coord -> Coord) -> GridState -> GridState
onAntCoord f gs = gs { antCoord = f $ antCoord gs }

onAntDir :: (Direction -> Direction) -> GridState -> GridState
onAntDir f gs = gs { antDir = f $ antDir gs }

onRules :: (Rules -> Rules) -> GridState -> GridState
onRules f gs = gs { rules = f $ rules gs }

-- }}}

-- Stepping {{{

nextDir :: Rules -> Color -> Maybe (Direction -> Direction)
nextDir rs col = M.lookup col (dirMap rs)

stepDir :: Direction -> Coord -> Coord
stepDir d (x,y) = case d of
  N -> (x,y-1)
  W -> (x-1,y)
  E -> (x+1,y)
  S -> (x,y+1)

step :: GridState -> GridState
step = stepState undefined undefined

stepState :: ViewPort -> Float -> GridState -> GridState
stepState _ dt gs@(GridState g curCrd@(x,y) curDir ss t rs)
  | x >= 0 &&  x < X gridSize && y >= 0 && y < Y gridSize = case mNextDir of
    Nothing -> error $ "Bad color for our rules: " ++ show curCol
    Just f  -> let
      newDir = f curDir
      newCrd = stepDir newDir curCrd
      in
      GridState g' newCrd newDir (ss + 1) (dt + t) rs
  | otherwise = gs
  where
  curCol = g # curCrd
  newCol = nextColor rs curCol
  g' = inGrid curCrd (const newCol) g
  mNextDir = nextDir rs curCol

-- }}}

-- Gloss {{{

render :: GridState -> Picture
render gs = pictures
  [ translate (-600) 0 $ scale 0.5 0.5 $ color white $ text txt
  , translate (-midPoint) (-midPoint) .
    pictures . concatMap elems $
    elems . mapCoord pixel $ grid gs
  ]
  where
  txt = "Steps: " ++ show (steps gs) ++ " Time: " ++ show (time gs)

pixel :: Coord -> Color -> Picture
pixel (x,y) c
  | c /= black = scale size size $ translate dx dy $
    color c $ rectangleSolid 1 1
  | otherwise = blank
  where
  size = toEnum $ fromEnum pixelSize
  dx = toEnum $ fromEnum $ unX x
  dy = toEnum $ fromEnum $ unY y

pixelSize :: Integer
pixelSize = 2
gridSize :: Integer
gridSize = 301

midPoint :: Float
midPoint = (toEnum $ fromEnum $ pixelSize * gridSize) / 2

simpleRules :: Rules
simpleRules = mkRules
  [ ( black , left  )
  , ( white , right )
  ]

noHighwayRules :: Rules
noHighwayRules = mkRules
  [ ( black , left  )
  , ( white , right )
  , ( azure , left  )
  ]

crazyRules :: Rules
crazyRules = mkRules
  [ ( black      , left  )
  , ( white      , left  )
  , ( red        , right )
  , ( green      , right )
  , ( orange     , right )
  , ( blue       , left  )
  , ( cyan       , right )
  , ( magenta    , left  )
  , ( yellow     , right )
  , ( rose       , left  )
  , ( aquamarine , left  )
  , ( azure      , right )
  ]

rules2 :: Rules
rules2 = mkRules
  [ ( black      , right )
  , ( white      , right )
  , ( red        , left  )
  , ( green      , left  )
  , ( orange     , left  )
  , ( blue       , right )
  , ( cyan       , left  )
  , ( magenta    , left  )
  , ( yellow     , left  )
  , ( rose       , right )
  , ( aquamarine , right )
  , ( azure      , right )
  ]

rules3 :: Rules
rules3 = mkRules
  [ ( black      , left  )
  , ( white      , right )
  , ( red        , right )
  , ( green      , right )
  , ( orange     , right )
  , ( blue       , right )
  , ( cyan       , left  )
  , ( magenta    , left  )
  , ( yellow     , right )
  ]

mkRules :: [(Color,Direction -> Direction)] -> Rules
mkRules cs = Rules (M.fromList cs) (map fst cs)

main = do
  simulate (InWindow "Lifespan" (800, 600) (0,0))
           black           -- background color
           100000          -- number of steps per second
           gState          -- initial world
           render          -- function to convert world to a Picture
           stepState       -- function to step the world one iteration
  where
  gState = initGridState (gridSize,gridSize) noHighwayRules

-- }}}

-- Array helpers {{{

imap :: (Ix i) => (i -> a -> b) -> Array i a -> Array i b
imap f a = fmap (uncurry f) a'
  where
  a' = array (bounds a) (map dupInd $ assocs a)
  dupInd p@(i,_) = (i,p)

succN :: Enum e => e -> Int -> e
succN e 0 = e
succN e n = succN (succ e) (n - 1)

slice :: (Ix i,Enum i) => i -> Int -> Array i a -> Array i a
slice i n a = array (start,end) es
  where
  (mn,mx) = bounds a
  start = min i mn
  end = max mx (succN start n)
  es = take n $ drop (fromEnum start) $ assocs a

-- }}}

