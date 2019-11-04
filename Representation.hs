-- Representation module
module Representation where
import qualified Text.Show
import qualified Data.List as DL
import qualified Data.Char
import Text.Regex.Posix
-- Command is a combination of initial piece position and final postion - encoded as a string
type Command = String
-- A message is a string that can be show(n)
type Message = String
-- A position is (rank,file) pair
type Position = (Int, Int)
-- Check valid positions 
checkValidPosition :: Position -> Bool
checkValidPosition (rank, file) = (rank >=0 && rank < 8) && (file >= 0 && file < 8)
-- Empty Position?
emptyPosition:: Board -> Position -> Bool
emptyPosition board position = emptyPiece (getPieceOnBoard board position)
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
-- get the name of the piece 
getPieceName :: Piece -> PieceName
getPieceName (p,_,_,_) = p
-- Is this the first time the piece is moved?
hasPieceMoved:: Piece -> Bool
hasPieceMoved (p,_,_,moved) = moved
-- Does a piece belong to a given player? 
pieceOwner::Player -> Piece  -> Bool
pieceOwner player (p,_,_,_) 
 | player == whitePlayer = Data.Char.isUpper p
 | player == blackPlayer = Data.Char.isLower p
 | otherwise = False
-- A rank 
type Rank = Int
-- A board is an arrangement of list of pieces in ranks (8 ranks)
type Board = [(Rank, [Piece])]
-- Get Piece on a board
getPieceOnBoard:: Board -> Position -> Piece
getPieceOnBoard board (rank, file) = (snd (board !! rank)) !! file
-- State is the Board Positions occupied by both players and the the current player
data State = State {board :: Board, player :: Player} deriving (Eq)
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

-- Step - how far have you moved from your original position
-- with respect to both Rank and File? 
type Step = (Int, Int)
-- Chess Pieces 
pawn (p,_,_,_) = p == 'P' || p == 'p'
rook (r,_,_,_) = r == 'R' || r == 'r'
knight (n,_,_,_) = n == 'N' || n == 'n'
bishop (b,_,_,_) = b == 'B' || b == 'b'
queen (q,_,_,_) = q == 'Q' || q == 'q'
king (k,_,_,_) = k == 'K' || k == 'k'
emptyPiece (e,_,_,_) = e == '.'
-- Get piece position
getPiecePosition :: Piece -> Position
getPiecePosition (_,pos,_,_) = pos
-- Players 
-- White player owns the pieces indicated in capital letters
whitePlayer = "white"
-- Black player is indicated by lowercase pieces 
blackPlayer = "black"
-- No player means that a position belongs to no player 
noplayer = "_"
-- Find out the your opponent
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
-- test0 is for convinience- for testing
-- A rank is a whole row
-- makeRank takes a row number and provides an initial state for that row
makeRank:: Int -> [Piece]
makeRank 0 = DL.zip4 ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'] (zip [0,0..] [0..7]) (replicate 8 blackPlayer) (replicate 8 False)
makeRank 1 = DL.zip4 (replicate 8 'p') (zip [1,1..] [0..7])   (replicate 8 blackPlayer) (replicate 8 False)
makeRank 2 = DL.zip4 (replicate 8 '.') (zip [2,2..] [0..7]) (replicate 8 noplayer) (replicate 8 False)
makeRank 3 = DL.zip4 (replicate 8 '.') (zip [3,3..] [0..7]) (replicate 8 noplayer) (replicate 8 False)
makeRank 4 = DL.zip4 (replicate 8 '.') (zip [4,4..] [0..7]) (replicate 8 noplayer) (replicate 8 False)
makeRank 5 = DL.zip4 (replicate 8 '.') (zip [5,5..] [0..7])  (replicate 8 noplayer) (replicate 8 False)
makeRank 6 = DL.zip4 (replicate 8 'P') (zip [6,6..] [0..7])  (replicate 8 whitePlayer) (replicate 8 False)
makeRank 7 = DL.zip4 ['R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R'] (zip [7,7..] [0..7]) (replicate 8 whitePlayer) (replicate 8 False)
