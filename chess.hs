import qualified Text.Show
-- Command is a combination of initial piece position and final postion - encoded as a string
type Command = String

-- A message is a string that can be show(n)
type Message = String

-- A position is just an index in an array == Int
type Position = Int

-- A player is represented by a String 
type Player = String

-- Name is a piece is just a character
type PieceName = Char
-- A piece is a combination of the name, position and the player who owns the piece
type Piece = (PieceName, Position, Player)

-- A rank 
type Rank = Int

-- A board is an arrangement of pieces
type Board = [(Rank, [Piece])]

-- State is the Board Positions occupied by both players and the the current player
data State = State Board Player


-- Implement show method to display the state
instance Show State where
  show (State board player)
   | player == whitePlayer =
     -- Print the board facing the white player
     "\n" ++ (foldr (++) "" (map rankToString board)) ++ (replicate 45 '_') ++ "\n" ++ [' ', '|'] ++  "    A    B    C    D    E    F    G    H " 
   | otherwise =
     -- Print the board facing the black player 
     "\n" ++ (foldr (++) "" (map rankToString (reverse board))) ++ (replicate 45 '_') ++ "\n" ++ [' ', '|'] ++  (reverse "    A    B    C    D    E    F    G    H    ")
   where
    rankToString (rn, rank) =  (foldl (\acc (n,_,_) -> acc ++ " " ++ (Text.Show.show n) ++ " ") (" |" ++ (Text.Show.show rn) ++ "|") (reverse rank)) ++ "\n\n"

-- Players 
-- White player owns the pieces indicated in capital letters
whitePlayer = "white"
-- Black player is indicated by lowercase pieces 
blackPlayer = "black"
-- No player means that a position belongs to no player 
noplayer = "_"

-- Initial state 
state0:: State
state0 = 
   -- To create a new state, make the 8 ranks and assign each of them a number
   -- Then make the white player the one to go first 
  State (zip [1..8] (map makeRank [1..8])) whitePlayer

-- A rank is a whole row
-- makeRank takes a row number and provides a default initial state for that row
makeRank:: Int -> [Piece]
makeRank 1 = zip3 ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'] [1..8] (replicate 8 blackPlayer)
makeRank 2 = zip3 (replicate 8 'p') [9..16]   (replicate 8 blackPlayer)
makeRank 3 = zip3 (replicate 8 '.') [17..24]  (replicate 8 noplayer)
makeRank 4 = zip3 (replicate 8 '.') [25..32]  (replicate 8 noplayer)
makeRank 5 = zip3 (replicate 8 '.') [33..40]  (replicate 8 noplayer)
makeRank 6 = zip3 (replicate 8 '.') [41..48]  (replicate 8 noplayer)
makeRank 7 = zip3 (replicate 8 'P') [48..56]  (replicate 8 whitePlayer)
makeRank 8 = zip3 ['R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R'] [57..64] (replicate 8 whitePlayer)

-- step :: State -> Command -> (Message, Maybe State)
-- step ... ... = ...

-- main :: IO ()
-- main = loop $ Just state0
--   where loop Nothing = return ()
--         loop (Just s) = do c <- getLine
--                            let (m, ms) = step s c
--                            putStrLn m
--                            loop ms