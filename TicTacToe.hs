data Player = X | O deriving (Eq, Show, Read)

type Board = [[Maybe Player]]

type Pos = (Int, Int)

showMaybePlayer :: Maybe Player -> String
showMaybePlayer Nothing = " "
showMaybePlayer (Just p)  = show p

-- |Shows a board on a genral form like this:
-- +-+-+-+
-- |X|O|X|
-- +-+-+-+
-- |O|O| |
-- +-+-+-+
-- | |X|O|
-- +-+-+-+
showBoard :: Board -> String
showBoard [[]] = ""
showBoard b    = foldl
              (\acc xs -> acc ++ '\n': foldl (\acc x -> acc ++ x ++ "|") "|" (map showMaybePlayer xs) ++ '\n':horizontalLine)
              horizontalLine
              b
              where
                horizontalLine :: String
                horizontalLine = '+' : take ((length $ b !! 0)*2) (cycle "-+")

change :: Int -> (a -> a) -> [a] -> [a]
change _ _ [] = []
change 0 f (x:xs) = f x : xs
change n f (x:xs) = x : change (n-1) f xs

set :: Int -> a -> [a] -> [a]
set n e xs = change n (const e) xs

legalMove :: Pos -> Board -> Bool
legalMove pos board
  | (fst pos) `elem` [0..2] && (snd pos) `elem` [0..2] = False
  | board !! snd pos !! fst pos /= Nothing = False
  | otherwise = True

placePlayer :: Pos -> Board -> Player -> Board
placePlayer pos board player = change (snd pos) (set (fst pos) (Just player)) board

move :: Pos -> Board -> Player -> Maybe Board
move pos board player
  | legalMove pos board = Just $ placePlayer pos board player
  | otherwise = Nothing

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
    Just b  -> do
      return $ b
    
play :: Board -> IO ()
play board = do
  putStrLn $ showBoard board

  let newBoard = getMove board
  
  newBoard >>= play

main = do
  putStrLn "Welcome to TicTacToe!\n"
  putStrLn "To make a move, write a position on the form (x,y), where (0,0) is the top left position."

  play startBoard
  
