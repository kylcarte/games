{-# LANGUAGE MultiWayIf #-}

module Games.Cribbage where

import Prelude as P
import qualified Data.Map as M

data Suit
  = Hearts
  | Clubs
  | Diamonds
  | Spades
  deriving (Eq,Show)

data Rank
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Eq,Ord,Enum,Show)

data Card = Card
  { rank :: Rank
  , suit :: Suit
  } deriving (Eq,Show)

points :: [Card] -> Int
points cs = sum
  [ runs rm
  , pairs rm
 -- , fifteens fm
  ]
  where
  rm = rankGroups cs

-- Runs {{{

runs :: [(Rank,Int)] -> Int
runs = sum . map runPoints . adjacents

runPoints :: (Rank,Rank,Int) -> Int
runPoints (l,h,w)
  | diff >= 2 = (diff + 1) * w
  | otherwise = 0
  where
  diff = hInt - lInt
  lInt = fromEnum l
  hInt = fromEnum h

adjacents :: [(Rank,Int)] -> [(Rank,Rank,Int)]
adjacents rm = foldr f [] rm
  where
  f (r,i) = loop (r,i)
  loop (r,i) im = case im of
    [] -> [(r,r,i)]
    (l,h,i') : rest
      | r == pred l -> (r,h,i * i') : rest
      | r == succ h -> (l,r,i * i') : rest
      | otherwise   -> (l,h,i) : loop (r,i) rest

-- }}}

-- Pairs {{{

pairs :: [(Rank,Int)] -> Int
pairs = sum . map pairPoints

pairPoints :: (Rank,Int) -> Int
pairPoints (_,i)
  | i > 1     = (i `choose` 2) * 2
  | otherwise = 0

--      n!
-- -------------
-- k! * (n - k)!

choose :: Int -> Int -> Int
n `choose` k = fac n `div` (fac k * fac (n - k))

fac :: Int -> Int
fac n 
  | n `elem` [0,1] = 1
  | n > 1          = n * fac (n - 1)
  | otherwise      = error "negative factorial"

-- }}}

-- Fifteens {{{

-- TODO: count fifteens

-- }}}

rankGroups :: [Card] -> [(Rank,Int)]
rankGroups cs = M.toList $ foldr f M.empty cs
  where
  f (Card r _) = flip M.alter r $ \mi -> case mi of
    Just i -> Just (i + 1)
    Nothing -> Just 1

testCards :: [Card]
testCards =
  [ Card Ace Spades
  , Card Two Hearts
  , Card Two Diamonds
  , Card Three Clubs
  , Card Four Spades
  ]

