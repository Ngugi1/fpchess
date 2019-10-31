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
data State = State {board :: Board, player :: Player} deriving (Eq)


-- Chess Pieces 
pawn (p,_,_,_) = p == 'P' || p == 'p'
rook (r,_,_,_) = r == 'R' || r == 'r'
knight (n,_,_,_) = n == 'N' || n == 'n'
bishop (b,_,_,_) = b == 'B' || b == 'b'
queen (q,_,_,_) = q == 'Q' || q == 'q'
king (k,_,_,_) = k == 'K' || k == 'k'
emptyPiece (e,_,_,_) = e == '.'



-- Implement show method to display the state
instance Show State where
  show (State board player)
   | player == whitePlayer =
     -- Print the board facing the white player
     "\n" ++ (foldl (++) "" (map rankToStringWhite board)) ++ (replicate 43'_') ++ "\n" ++ [' ', '|'] ++  "    A    B    C    D    E    F    G    H " 
   | otherwise =
     -- Print the board facing the black player 
     "\n" ++ (foldl (++) "" (map rankToStringBlack (reverse board))) ++ (replicate 43 '_') ++ "\n" ++ [' ', '|'] ++  (reverse "    A    B    C    D    E    F    G    H    ")
   where
    rankToStringWhite (rn, rank) =  (foldl (\acc (n,_,_,_) -> acc ++ " " ++ (Text.Show.show n) ++ " ") (" |" ++ (Text.Show.show (rn + 1)) ++ "|")  rank) ++ "\n\n"
    rankToStringBlack (rn, rank) = (foldl (\acc (n,_,_,_) -> acc ++ " " ++ (Text.Show.show n) ++ " ") (" |" ++ (Text.Show.show (rn + 1)) ++ "|") (reverse rank)) ++ "\n\n"
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
  State (zip [0..7] (map makeRank [0..7])) whitePlayer


-- A rank is a whole row
-- makeRank takes a row number and provides a default initial state for that row
makeRank:: Int -> [Piece]
makeRank 0 = DL.zip4 ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'] (zip [0,0..] [0..7]) (replicate 8 blackPlayer) (replicate 8 False)
makeRank 1 = DL.zip4 (replicate 8 'p') (zip [1,1..] [0..7])   (replicate 8 blackPlayer) (replicate 8 False)
makeRank 2 = DL.zip4 (replicate 8 '.') (zip [2,2..] [0..7]) (replicate 8 noplayer) (replicate 8 False)
makeRank 3 = DL.zip4 (replicate 8 '.') (zip [3,3..] [0..7]) (replicate 8 noplayer) (replicate 8 False)
makeRank 4 = DL.zip4 (replicate 8 '.') (zip [4,4..] [0..7]) (replicate 8 noplayer) (replicate 8 False)
makeRank 5 = DL.zip4 (replicate 8 '.') (zip [5,5..] [0..7])  (replicate 8 noplayer) (replicate 8 False)
makeRank 6 = DL.zip4 (replicate 8 'P') (zip [6,6..] [0..7])  (replicate 8 whitePlayer) (replicate 8 False)
makeRank 7 = DL.zip4 ['R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R'] (zip [7,7..] [0..7]) (replicate 8 whitePlayer) (replicate 8 False)



-- Validate command is in right format  
validCommand:: Command -> Bool
validCommand command =  command =~ "[a-h][1-8][a-h][1-8]" && (length command == 4)

-- Convert a letter to an int
fileToInt:: Char -> Int
fileToInt 'a' = 0
fileToInt 'b' = 1
fileToInt 'c' = 2
fileToInt 'd' = 3
fileToInt 'e' = 4
fileToInt 'f' = 5
fileToInt 'g' = 6
fileToInt 'h' = 7

-- Get the rank and file of current and target position from the command
getMove:: Command -> Move
getMove command =
  let fromFile = (fileToInt $ command !! 0)
      fromRank = read [(command !! 1)] - 1 -- Give the read some context 
      toFile = (fileToInt $ command !! 2)
      toRank = read  [(command !! 3)] - 1 -- Give read a context - we need this to ne Int
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
play :: State -> Piece
play state = getPieceOnBoard (board state) (fst (getMove "a7a5"))

-- Make a move
makeMove:: State -> Piece -> To -> State
makeMove state (name, position, player, moved) to =
   State  (map (replacePiece to (name, to, player, True)) (map (removePiece position) (board state))) (otherPlayer player)
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
 | validCommand command == True && validMove state piece moveToBeMade currentPlayer  =
  -- check if it is a valid move for the appropriate piece
  ("🎊🎊🎊 Nice Move!!\n" ++ show (otherPlayer currentPlayer) ++ ", it is your turn",
  Just $ makeMove state piece newPosition)
 | otherwise = ("Invalid command, try again", Just state)
 where piece = (getPieceOnBoard (board state) (fst moveToBeMade))
       moveToBeMade = getMove command
       newPosition = snd moveToBeMade
       currentPlayer = player state

-- Valid moves - rules 
validMove:: State -> Piece -> Move -> Player -> Bool 
validMove state piece move player
 | rook piece = True -- Do rook  
 | knight piece =True -- Knight
 | bishop piece =True -- Bishop stuff
 | queen piece =True -- Queen stuff
 | king piece = True-- King stuff 
 | pawn piece = checkPawnMove state move player -- Pawn rules applied here
 | otherwise = False

checkPawnMove:: State -> Move -> Player -> Bool
checkPawnMove state move player =   (verticalMove move) && (forwardMove player move) && (emptyPosition (board state) (snd move))
-- ##### Movements ####
-- Forward move
forwardMove:: Player -> Move -> Bool
forwardMove player ((fromRank, fromFile), (toRank, toFile))
 |player == whitePlayer = (toRank < fromRank)
 |player == blackPlayer =  (toRank > fromRank)
 |otherwise = False

 -- Vertical move 
verticalMove :: Move -> Bool
verticalMove ((fromRank, fromFile), (toRank, toFile)) = (fromRank /= toRank) && (fromFile == toFile)

-- Horizontal move
horizontalMove:: Move -> Bool
horizontalMove ((fromRank, fromFile), (toRank, toFile)) = fromRank == toRank && fromFile /= toFile


-- ###Diagonal## ---
-- Diagonal move
diagonalMove :: Move -> Bool
diagonalMove ((fromRank, fromFile), (toRank, toFile)) = 
  abs fromRank - toRank  == abs fromFile - toFile

-- Generate all possible diagonal pieces relative to a position (with that position excluded)
diagonalPositions:: Position -> [Position]
diagonalPositions (rank, file) =
  topLeft ++ topRight ++ bottomLeft ++ bottomRight
  where topLeft = zip [rank-1 ,(rank-2) .. 1] [file-1, (file-2) .. 1] -- Generate one step away from current position
        topRight = zip [rank-1, (rank-2) .. 1] [(file+1) .. 8]
        bottomLeft = zip [(rank + 1) .. 8] [file-1, (file-2) ..]
        bottomRight = zip [(rank + 1) ..8] [(file+1)..8]

-- Empty Position?
emptyPosition:: Board -> Position -> Bool
emptyPosition board position = emptyPiece $ getPieceOnBoard board position



main :: IO ()
main = loop $ Just state0
  where loop Nothing = return()
        loop (Just s) =
          do
            putStrLn (if s == state0 then show s ++ "\n\n 😉 " ++whitePlayer ++ ",it is your turn" else "")
            c <- getLine
            let (m, ms) = step s c
            putStrLn $ show ms
            putStrLn m
            loop ms


