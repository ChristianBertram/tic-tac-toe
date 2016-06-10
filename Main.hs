import Board

startBoard :: Board
startBoard = [[Nothing, Nothing, Nothing]
             ,[Nothing, Nothing, Nothing]
             ,[Nothing, Nothing, Nothing]
             ]

getMove :: Board -> IO Board
getMove board = do
  putStr "\nYour move: "
  pos <- getLine

  maybe retry return (move (read pos) board X)
    where
      retry = do
        putStrLn "Illegal move."
        getMove board

play :: Board -> IO ()
play board = do
  putStrLn $ showBoard board

  let newBoard = getMove board
  newBoard >>= play

main :: IO ()
main = do
  putStrLn "Welcome to TicTacToe!\n"
  putStrLn "To make a move, write a position on the form (x,y), where (0,0) is the top left position."

  play startBoard
