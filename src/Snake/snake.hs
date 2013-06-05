{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Display

import Control.Applicative
import System.Random

data Game
  = Ready Coord StdGen
  | Running Env
  | Over Coord StdGen

-- Env {{{

newtype X = X { fromX :: Int } deriving (Eq,Ord,Show,Enum,Num,Random)
newtype Y = Y { fromY :: Int } deriving (Eq,Ord,Show,Enum,Num,Random)
type Coord = (X,Y)

data Direction = N | W | E | S deriving (Eq,Ord,Show,Enum)

instance Random Direction where
  random g = let (i,g') = randomR (0 :: Int,3) g in ([N .. S] !! i,g')
  randomR (lo,hi) g = let (i,g') = randomR (fromEnum lo,fromEnum hi) g in ([lo .. hi] !! i, g')

data Env = Env
  { sHead    :: Coord
  , sDir     :: Direction
  , sTail    :: [Coord]
  , gridSize :: Coord
  , apple    :: Coord
  , appleGen :: StdGen
  }

mkInitEnv :: Coord -> StdGen -> Env
mkInitEnv m@(mx,my) gen0 = Env hd dr [hd,tl] m ap gen3
  where
  (tl,gen1) = randomCenterTile gen0
  (dr,gen2) = random  gen1
  hd = takeStep dr tl
  (ap,gen3) = randomApple m gen2

randomCenterTile :: StdGen -> (Coord,StdGen)
randomCenterTile gen0 = ((x,y),gen2)
  where
  (x,gen1) = randomR (10,20) gen0
  (y,gen2) = randomR (10,20) gen1

randomApple :: Coord -> StdGen -> (Coord,StdGen)
randomApple (mx,my) gen0 = ((apX,apY),gen2)
  where
  (apX,gen1) = randomR (0,mx-1) gen0
  (apY,gen2) = randomR (0,my-1) gen1

takeStep :: Direction -> Coord -> Coord
takeStep d (x,y) = case d of
  N -> (x,y+1)
  W -> (x-1,y)
  E -> (x+1,y)
  S -> (x,y-1)

keepTail :: Coord -> [Coord] -> [Coord]
keepTail new = init . growTail new

growTail :: Coord -> [Coord] -> [Coord]
growTail = (:)

-- }}}

-- Stepping {{{

out :: (Ord a,Num a) => a -> a -> Bool
n `out` mx = n < 0 || n >= mx

stepGame :: Float -> Game -> Game
stepGame _ g = case g of
  Running env -> maybe (Over (gridSize env) (appleGen env)) Running $ stepEnv env
  _           -> g

stepEnv :: Env -> Maybe Env
stepEnv env@(Env hd@(x,y) dr tl m@(mx,my) ap gen)
  | x `out` mx || y `out` my = Nothing
  | hd == ap                 = Just $ Env hd' dr tl'' m ap' gen'
  | otherwise                = Just $ Env hd' dr tl' m ap gen
  where
  hd' = takeStep dr hd
  tl' = keepTail hd' tl
  tl'' = growTail hd' tl
  (ap',gen') = randomApple m gen

random2 :: (Num a,Num b,Random a,Random b,RandomGen g)
  => g -> ((a,b),g)
random2 g = ((a,b),g'')
  where
  (a,g')  = random g
  (b,g'') = random g'

randomR2 :: (Num a,Num b,Random a,Random b,RandomGen g)
  => (a,b) -> g -> ((a,b),g)
randomR2 (hiA,hiB) g = ((a,b),g'')
  where
  (a,g')  = randomR (0,hiA) g
  (b,g'') = randomR (0,hiB) g'

-- }}}

-- Keys {{{

handleKeys :: Event -> Game -> Game
handleKeys e g = case g of
  Ready crd gen -> readyKeys e crd gen
  Running env   -> runningKeys e env
  Over crd gen -> overKeys e crd gen

readyKeys :: Event -> Coord -> StdGen -> Game
readyKeys e crd gen = case e of
  EventKey (SpecialKey KeySpace) Down mods _ 
    | mods == noMods -> startGame crd gen
  _ -> Ready crd gen

runningKeys :: Event -> Env -> Game
runningKeys e env = case e of
  EventKey (Char 'q') Down mods _
    | mods == noMods -> Over (gridSize env) (appleGen env)
  EventKey (Char 'w') Down mods _
    | mods == noMods -> Running $ goNorth env
  EventKey (Char 'a') Down mods _
    | mods == noMods -> Running $ goWest  env
  EventKey (Char 's') Down mods _
    | mods == noMods -> Running $ goEast  env
  EventKey (Char 'd') Down mods _
    | mods == noMods -> Running $ goSouth env
  EventKey (SpecialKey KeyUp)    Down mods _
    | mods == noMods -> Running $ goNorth env
  EventKey (SpecialKey KeyLeft)  Down mods _
    | mods == noMods -> Running $ goWest  env
  EventKey (SpecialKey KeyRight) Down mods _
    | mods == noMods -> Running $ goEast  env
  EventKey (SpecialKey KeyDown)  Down mods _
    | mods == noMods -> Running $ goSouth env
  _ -> Running env

overKeys :: Event -> Coord -> StdGen -> Game
overKeys e crd gen = case e of
  EventKey (Char 'r') Down mods _
    | mods == noMods -> Ready crd gen
  _ -> Over crd gen

changeDir :: Direction -> Direction -> Env -> Env
changeDir dir opp env@(Env hd dr tl m ap gen) =
  Env hd (if dr == opp then dr else dir) tl m ap gen

goNorth :: Env -> Env
goNorth = changeDir N S

goWest :: Env -> Env
goWest = changeDir W E

goEast :: Env -> Env
goEast = changeDir E W

goSouth :: Env -> Env
goSouth = changeDir S N

-- }}}

-- Render {{{

pixel :: Color -> Coord -> Coord -> Picture
pixel c m@(mx,my) (cx,cy) =
  translate (pixelSize / 2) (pixelSize / 2) $
  scale pixelSize pixelSize $
  translate (x0 + cx') (y0 + cy') $
  color c $ rectangleSolid 1 1
  where
  x, y, x0, x1, y0, y1 :: Float
  x = toEnum $ fromX mx
  y = toEnum $ fromY my
  x1 = x / 2
  y1 = y / 2
  x0 = -x1
  y0 = -y1
  cx' = toEnum (fromX cx)
  cy' = toEnum (fromY cy)

render :: Game -> Picture
render g = case g of
  Ready m _ -> renderReady m
  Running env -> renderRunning env
  Over _ _ -> renderOver

renderGrid :: Coord -> Picture
renderGrid m@(mx,my) = color white $ scale pixelSize pixelSize $ pictures $
  verticals ++ horizontals
  where
  verticals = [ line [(x0 + dx,y0),(x0 + dx,y1)] | dx <- [0 .. x] ]
  horizontals = [ line [(x0,y0 + dy),(x1,y0 + dy)] | dy <- [0 .. y] ]
  x, y, x0, x1, y0, y1 :: Float
  x = toEnum $ fromX mx
  y = toEnum $ fromY my
  x1 = x / 2
  y1 = y / 2
  x0 = -x1
  y0 = -y1

renderApple = pixel red

renderSnake m tl = pictures $ map (pixel green m) tl

renderReady :: Coord -> Picture
renderReady m = pictures
  [ renderGrid m
  --, color white $ text "Press Space to Start"
  ]

renderRunning :: Env -> Picture
renderRunning env = pictures
  [ renderApple (gridSize env) (apple env)
  , renderSnake (gridSize env) (sTail env)
  , renderGrid $ gridSize env
  ]

renderOver :: Picture
renderOver = blank -- color white $ text "Game Over"

-- }}}

startGame :: Coord -> StdGen -> Game
startGame crd gen = Running $ mkInitEnv crd gen

noMods :: Modifiers
noMods = Modifiers Up Up Up

main :: IO ()
main = do
  game <- Ready (31,31) <$> getStdGen
  play (InWindow "Snake" (640,640) (0,0))
       black
       8
       game
       render
       handleKeys
       stepGame

readCoord :: String -> Maybe Coord
readCoord s = let ls = map read $ words s in case ls of
  [x,y] -> Just (X x,Y y)
  _     -> Nothing

pixelSize :: Float
pixelSize = 16

