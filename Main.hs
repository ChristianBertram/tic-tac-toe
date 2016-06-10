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

  let newBoard = move (read pos) board X
  case newBoard of
    Nothing -> do
      putStrLn "Illegal move."
      getMove board
    Just b  -> return b

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
