{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

import Data.List
import qualified Data.Map as M
import qualified Data.Vector as V

-- Types {{{

type Size = Coord
type Coord = (Int,Int)
type ColorMap = M.Map Color (Direction -> Direction)

newtype Row = Row { unRow :: V.Vector Color }
newtype Grid = Grid { unGrid :: V.Vector Row }

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

instance Show Row where
  show = ("| " ++) . (++ " |") . intercalate " , " . V.toList . V.map show . unRow

instance Show Grid where
  show g = unlines (cap : rs ++ [cap])
    where
    rs = V.toList $ V.map show $ unGrid g
    cap = "+" ++ replicate (l - 2) '-' ++ "+"
    l = case rs of
      []   -> 0
      r:_  -> length r

class IsVector v e | v -> e where
  vmap       :: (e -> e) -> v -> v
  imap       :: (Int -> e -> e) -> v -> v
  slice      :: Int -> Int -> v -> v
  vreplicate :: Int -> e -> v
  vlength    :: v -> Int
  (//)       :: v -> [(Int,e)] -> v
  (!)        :: v -> Int -> e

instance IsVector Row Color where
  vmap f (Row r) = Row $ V.map f r
  imap f (Row r) = Row $ V.imap f r
  slice i0 n (Row r) = Row $ V.slice i0 n r
  vreplicate i c = Row $ V.replicate i c
  vlength (Row r) = V.length r
  (Row r) // cs = Row $ r V.// cs
  (Row r) ! i = r V.! i

instance IsVector Grid Row where
  vmap f (Grid g) = Grid $ V.map f g
  imap f (Grid g) = Grid $ V.imap f g
  slice i0 n (Grid g) = Grid $ V.slice i0 n g
  vreplicate i r = Grid $ V.replicate i r
  vlength (Grid g) = V.length g
  (Grid g) // rs = Grid $ g V.// rs
  (Grid g) ! i = g V.! i

-- }}}

-- Grid Functions {{{

inRow :: Int -> (Color -> Color) -> Row -> Row
inRow y f r = r // [(y,c')]
  where
  c = r ! y
  c' = f c

inGrid :: Coord -> (Color -> Color) -> Grid -> Grid
inGrid (x,y) f g = g // [(y,r')]
  where
  r = g ! y
  r' = inRow x f r

subGrid :: Coord -> Size -> Grid -> Grid
subGrid (x,y) (sx,sy) = vmap (slice y sy) . slice x sx

mapCoord :: (Coord -> Color -> Color) -> Grid -> Grid
mapCoord f = imap $ \y -> imap $ \x -> f (x,y)

solid :: Size -> Color -> Grid
solid (sx,sy) = vreplicate sy . vreplicate sx

blankGrid :: Size -> Grid
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
  { grid      :: Grid
  , antCoord  :: Coord
  , antDir    :: Direction
  , steps     :: Int
  , rules     :: Rules
  }

instance Show GridState where
  show (GridState g c d _ _) = unlines
    [ show c
    , show d
    , show g
    ]

initGridState :: Size -> Rules -> GridState
initGridState size@(sx,sy) = GridState (blankGrid size) (x,y) E 0
  where
  x = sx `div` 2
  y = sy `div` 2

onGrid :: (Grid -> Grid) -> GridState -> GridState
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
stepState _ _ gs@(GridState g curCrd@(x,y) curDir ss rs)
  | x >= 0 &&  x < gridSize && y >= 0 && y < gridSize = case mNextDir of
    Nothing -> error $ "Bad color for our rules: " ++ show curCol
    Just f  -> let
      newDir = f curDir
      newCrd = stepDir newDir curCrd
      in
      GridState g' newCrd newDir (ss + 1) rs
  | otherwise = gs
  where
  curCol = g ! y ! x
  newCol = nextColor rs curCol
  g' = inGrid curCrd (const newCol) g
  mNextDir = nextDir rs curCol

-- }}}

-- Gloss {{{

render :: GridState -> Picture
render g = pictures
  [ translate (-1000) 0 $ scale 0.5 0.5 $ color white $ text $ show (steps g)
  , translate (-midPoint) (-midPoint) .
    pictures . concatMap V.toList .
    V.toList . mapCoords pixel $ grid g
  ]

mapCoords :: ((Float,Float) -> Color -> a) -> Grid -> V.Vector (V.Vector a)
mapCoords f = V.imap (\y -> V.imap (\x -> f (toEnum x,toEnum y)) . unRow) . unGrid

pixel :: (Float,Float) -> Color -> Picture
pixel (x,y) c
  | c /= black = pictures $ map (translate dx dy)
    [ color c $ rectangleSolid size size
    , color black $ rectangleWire size size
    ]
  | otherwise = blank
  where
  size = toEnum pixelSize
  dx = size * x
  dy = size * y

pixelSize :: Int
pixelSize = 2
gridSize :: Int
gridSize = 301

midPoint :: Float
midPoint = toEnum (pixelSize * gridSize) / 2

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
           1000            -- number of steps per second
           gState          -- initial world
           render          -- function to convert world to a Picture
           stepState       -- function to step the world one iteration
  where
  gState = initGridState (gridSize,gridSize) rules3

-- }}}

