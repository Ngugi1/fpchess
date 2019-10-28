import qualified Text.Show
import qualified Data.List as DL
import Text.Regex.Posix
-- Command is a combination of initial piece position and final postion - encoded as a string
type Command = String

-- A message is a string that can be show(n)
type Message = String

-- A position is (rank,file) pair
type Position = (Int, Int)

-- Position being vacated by player 
type From = Position

-- Target position of the player
type To = Position

-- Move is current position and the target position of a piece on the board
type Move = (From, To)

-- A player is represented by a String 
type Player = String

-- Name is a piece is just a character
type PieceName = Char

-- Moved - indicates if a piece has been moved 
type Moved = Bool


-- A piece is a combination of the name, position, the player and a flag to see if a piece has been moved
type Piece = (PieceName, Position, Player, Moved)

-- A rank 
type Rank = Int

-- A board is an arrangement of pieces
type Board = [(Rank, [Piece])]

-- State is the Board Positions occupied by both players and the the current player
data State = State {board :: Board, player :: Player}


-- Implement show method to display the state
instance Show State where
  show (State board player)
   | player == whitePlayer =
     -- Print the board facing the white player
     "\n" ++ (foldl (++) "" (map rankToString board)) ++ (replicate 43'_') ++ "\n" ++ [' ', '|'] ++  "    A    B    C    D    E    F    G    H " 
   | otherwise =
     -- Print the board facing the black player 
     "\n" ++ (foldl (++) "" (map rankToString (reverse board))) ++ (replicate 43 '_') ++ "\n" ++ [' ', '|'] ++  (reverse "    A    B    C    D    E    F    G    H    ")
   where
    rankToString (rn, rank) =  (foldl (\acc (n,_,_,_) -> acc ++ " " ++ (Text.Show.show n) ++ " ") (" |" ++ (Text.Show.show rn) ++ "|") (reverse rank)) ++ "\n\n"

-- Players 
-- White player owns the pieces indicated in capital letters
whitePlayer = "white"
-- Black player is indicated by lowercase pieces 
blackPlayer = "black"
-- No player means that a position belongs to no player 
noplayer = "_"

otherPlayer::String -> String
otherPlayer player
 | player == blackPlayer = whitePlayer
 | otherwise = blackPlayer

-- Initial state 
state0:: State
state0 = 
   -- To create a new state, make the 8 ranks and assign each of them a number
   -- Then make the white player the one to go first 
  State (zip [1..8] (map makeRank [1..8])) whitePlayer

-- A rank is a whole row
-- makeRank takes a row number and provides a default initial state for that row
makeRank:: Int -> [Piece]
makeRank 1 = DL.zip4 ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'] (zip [1,1..] [1..8]) (replicate 8 blackPlayer) (replicate 8 False)
makeRank 2 = DL.zip4 (replicate 8 'p') (zip [2,2..] [1..8])   (replicate 8 blackPlayer) (replicate 8 False)
makeRank 3 = DL.zip4 (replicate 8 '.') (zip [3,3..] [1..8]) (replicate 8 noplayer) (replicate 8 False)
makeRank 4 = DL.zip4 (replicate 8 '.') (zip [4,4..] [1..8]) (replicate 8 noplayer) (replicate 8 False)
makeRank 5 = DL.zip4 (replicate 8 '.') (zip [5,5..] [1..8]) (replicate 8 noplayer) (replicate 8 False)
makeRank 6 = DL.zip4 (replicate 8 '.') (zip [6,6..] [1..8])  (replicate 8 noplayer) (replicate 8 False)
makeRank 7 = DL.zip4 (replicate 8 'P') (zip [7,7..] [1..8])  (replicate 8 whitePlayer) (replicate 8 False)
makeRank 8 = DL.zip4 ['R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R'] (zip [8,8..] [1..8]) (replicate 8 whitePlayer) (replicate 8 False)



-- Validate command is in right format  
validCommand:: Command -> Bool
validCommand command =  command =~ "[a-h][1-8][a-h][1-8]" && (length command == 4)

-- Convert a letter to an int
fileToInt:: Char -> Int
fileToInt 'a' = 1
fileToInt 'b' = 2
fileToInt 'c' = 3
fileToInt 'd' = 4
fileToInt 'e' = 5
fileToInt 'f' = 6
fileToInt 'g' = 7
fileToInt 'h' = 8

-- Get the rank and file of current and target position from the command
getMove:: Command -> Move
getMove command =
  let fromFile = fileToInt $ command !! 0
      fromRank = read [(command !! 1)] + 0 -- Give the read some context 
      toFile = fileToInt $ command !! 2
      toRank = read  [(command !! 3)] + 0 -- Give read a context - we need this to ne Int
  in ((fromRank, fromFile), (toRank,toFile))

-- Get Piece 
getPieceOnBoard:: Board -> Position -> Piece
getPieceOnBoard board (rank, file) = (snd (board !! rank)) !! file

-- Check if the move is valid 
-- validMove :: Player -> Piece -> Move -> Bool


-- Remove a piece from a rank by replacing it with an empty piece
removePiece:: Position -> (Rank, [Piece]) -> (Rank, [Piece])
removePiece position pieces =
  replacePiece position ('.',position, noplayer, True) pieces

-- Play 
play :: State -> State
play state = 
  move state ('P', (7,1), "white", False) (5, 1)

-- Make a move
move:: State -> Piece -> To -> State
move state (name, position, player, moved) to =
   State (map (replacePiece to (name, position, player, moved)) (map (removePiece position) (board state))) (otherPlayer player)
-- Place a piece or replace at the position with the new piece 
replacePiece:: Position -> Piece -> (Rank, [Piece]) -> (Rank, [Piece])
replacePiece (rank, file) (pname,_,player,_) (rn, pieces) =
  (rn, map filterPiece pieces)
  where filterPiece (n,(r, f),p,m) = 
         if (rank == r && f == file)
         then (pname, (rank,file),player,True)
         else (n,(r, f),p,m)
-- TODO:: Step command  - continue here 
step :: State -> Command -> (Message, Maybe State)
step state command 
 | validCommand command == True = ("Move made", Just state)
 | otherwise = ("Invalid command, try again", Just state)

-- Valid moves - rules 
-- Valid moves for pawns
-- Pawns can only move forward, max two steps if the 
  

main :: IO ()
main = loop $ Just state0
  where loop Nothing = return ()
        loop (Just s) = do c <- getLine
                           let (m, ms) = step s c
                           putStrLn m
                           putStrLn c
                           putStrLn $ show $ play ms
                           loop ms


