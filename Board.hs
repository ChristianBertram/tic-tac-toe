module Board
( Player(..)
, Board
, Pos
, showBoard
, legalMove
, move
) where

import Data.Maybe

data Player = X | O deriving (Eq, Show, Read)

-- | Boards can have any rectangular dimensions
type Board = [[Maybe Player]]

type Pos = (Int, Int)

showMaybePlayer :: Maybe Player -> String
showMaybePlayer Nothing  = " "
showMaybePlayer (Just p) = show p

-- | intersperse (from Data.List) with the seperator also on both sides.
intersperseAll :: a -> [a] -> [a]
intersperseAll sep []     = [sep]
intersperseAll sep (x:xs) = sep : x : intersperseAll sep xs

intercalateAll :: [a] -> [[a]] -> [a]
intercalateAll xs xss = concat (intersperseAll xs xss)

innerLength :: [[a]] -> Int
innerLength xs
  | null xs = 0
  | otherwise = length $ head xs

-- |Shows a board on a genral form like this:
-- +-+-+-+
-- |X|O|X|
-- +-+-+-+
-- |O|O| |
-- +-+-+-+
-- | |X|O|
-- +-+-+-+
showBoard :: Board -> String
showBoard b = intercalateAll ('\n':sepLine) (map showLine b)
                where
                  width = innerLength b

                  sepLine :: String
                  sepLine = intersperseAll '+' (replicate width '-')

                  showLine :: [Maybe Player] -> String
                  showLine xs = '\n' : intercalateAll "|" (map showMaybePlayer xs)

change :: Int -> (a -> a) -> [a] -> [a]
change _ _ [] = []
change 0 f (x:xs) = f x : xs
change n f (x:xs) = x : change (n-1) f xs

set :: Int -> a -> [a] -> [a]
set n e = change n (const e)

inRange :: Int -> (Int, Int) -> Bool
inRange x (start, end) = x `elem` [start..end]

legalMove :: Pos -> Board -> Bool
legalMove pos board
  | not $ fst pos `inRange` (0, innerLength board-1)
    &&    snd pos `inRange` (0, length board-1)     = False
  | Data.Maybe.isJust (board !! snd pos !! fst pos) = False
  | otherwise = True

placePlayer :: Pos -> Board -> Player -> Board
placePlayer pos board player = change (snd pos) (set (fst pos) (Just player)) board

move :: Pos -> Board -> Player -> Maybe Board
move pos board player
  | legalMove pos board = Just $ placePlayer pos board player
  | otherwise = Nothing
