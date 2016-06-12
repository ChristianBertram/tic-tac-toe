import Board
import AI

import Data.Maybe
import Text.Read

emptyBoard :: Int -> Int -> Board
emptyBoard _ 0 = []
emptyBoard width height = replicate width Nothing : emptyBoard width (height-1)

getMove :: Board -> IO Board
getMove board = do
  putStr "\nYour move: "
  pos <- getLine

  case readMaybe pos of
    Nothing -> do
      putStrLn "Invalid syntax."
      getMove board
    Just pos' ->
      maybe retry return (move pos' board X)
        where
          retry = do
            putStrLn "Illegal move."
            getMove board

play :: Board -> IO ()
play board = do
  putStrLn $ showBoard board

  maybe
    (if fullBoard board
      then putStrLn "It's a tie!"
      else do
        let newBoard  = getMove board
        let newBoard' = fmap getAIMove newBoard
        newBoard' >>= play
    )
    (\x -> putStrLn $ show x ++ " won the game!")
    (winner board)

main :: IO ()
main = do
  putStrLn "Welcome to TicTacToe!\n"

  putStrLn "How wide should the board be? (Write a whole number)"
  width <- getLine
  putStrLn "How tall should the board be? (Write a whole number)"
  height <- getLine

  putStrLn "\nTo make a move, write a position on the form (x,y), where (0,0) is the top left position."

  play $ emptyBoard (read width) (read height)
