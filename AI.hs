module AI
(
getAIMove
) where

import Board

maximumFst :: Ord a => [(a, b)] -> (a, b)
maximumFst [] = error "empty list"
maximumFst xs = foldr (\x acc -> if fst x > fst acc then x else acc) (head xs) xs

evaluateBoard :: Board -> Player -> Int
evaluateBoard board player
  | winner' == Just X = -1 -- AI lost
  | winner' == Just O =  1 -- AI won
  | fullBoard board   =  0 -- tied
  | otherwise = (case player of X -> minimum; O -> maximum)
                  [evaluateBoard (safeMove (x, y) board player) nextPlayer
                  | x <- [0..(innerLength board -1)]
                  , y <- [0..(length board -1)]
                  , legalMove (x, y) board
                  ]
  where
    winner' = winner board
    nextPlayer
      | player == X = O
      | otherwise   = X

getAIMove :: Board -> Board
getAIMove board
  | fullBoard board = board
  | otherwise = snd . maximumFst $
                  [(evaluateBoard (safeMove (x, y) board O) X, safeMove (x, y) board O)
                  | x <- [0..(innerLength board -1)]
                  , y <- [0..(length board -1)]
                  , legalMove (x, y) board
                  ]
