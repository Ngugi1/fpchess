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
test0 :: State
test0 = 
  -- To create a new state, make the 8 ranks and assign each of them a number
   -- Then make the white player the one to go first 
  State (zip [0..7] (map makeRank1 [0..7])) whitePlayer


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

makeRank1:: Int -> [Piece]
makeRank1 0 = DL.zip4 ['r', '.', '.', '.', 'k', '.', '.', 'r'] (zip [0,0..] [0..7]) (replicate 8 blackPlayer) (replicate 8 False)
makeRank1 1 = DL.zip4 (replicate 8 'q') (zip [1,1..] [0..7])   (replicate 8 blackPlayer) (replicate 8 False)
makeRank1 2 = DL.zip4 (replicate 8 'n') (zip [2,2..] [0..7]) (replicate 8 blackPlayer) (replicate 8 False)
makeRank1 3 = DL.zip4 (replicate 8 'n') (zip [3,3..] [0..7]) (replicate 8 blackPlayer) (replicate 8 False)
makeRank1 4 = DL.zip4 (replicate 8 'P') (zip [4,4..] [0..7]) (replicate 8 whitePlayer) (replicate 8 False)
makeRank1 5 = DL.zip4 (replicate 8 'n') (zip [5,5..] [0..7])  (replicate 8 blackPlayer) (replicate 8 False)
makeRank1 6 = DL.zip4 (replicate 8 'n') (zip [6,6..] [0..7])  (replicate 8 blackPlayer) (replicate 8 True)
makeRank1 7 = DL.zip4 ['R', '.', 'q', '.', 'K', 'r', '.', 'R'] (zip [7,7..] [0..7]) (replicate 8 whitePlayer) (replicate 8 False)

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


-- Remove a piece from a rank by replacing it with an empty piece
removePiece:: Position -> (Rank, [Piece]) -> (Rank, [Piece])
removePiece position pieces =
  replacePiece position ('.',position, noplayer, True) pieces

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

-- Control the game from here
step :: State -> Command -> (Message, Maybe State)
step state command
 | pawn piece  && isValidMove = showSuccess  (postProcessMovedPawn state  piece moveToBeMade currentPlayer) currentPlayer
 | king piece && isValidMove = showSuccess (makeMove state piece newPosition) currentPlayer
 | isValidMove = showSuccess (makeMove state piece newPosition) currentPlayer
 | otherwise = showError state
 where currentBoard = (board state) 
       piece = (getPieceOnBoard currentBoard (fst moveToBeMade))
       moveToBeMade = getMove command
       currentPlayer = player state
       ownPiece = pieceOwner currentPlayer piece  -- You can't move other player's pieces
       playerMoved = (stepsMoved moveToBeMade) /= (0,0) -- A player must make a move
       newPosition = snd moveToBeMade
       isValidMove = validCommand command && ownPiece && playerMoved && validMove state piece moveToBeMade currentPlayer
-- Success message
showSuccess :: State  -> Player -> (Message, Maybe State)
showSuccess state currentPlayer = ("🎊🎊🎊 Nice Move!!\n" ++ show (otherPlayer currentPlayer) ++ ", it is your turn",
  Just $ state)

-- Failure message 
showError :: State -> (Message, Maybe State)
showError state = ("Invalid move/command, try again", Just state)

promotePawn :: Piece -> Move -> Player ->  Piece
promotePawn (name, pos, player, moved) move currentPlayer 
 | currentPlayer == whitePlayer &&  newRank == 0 = ('Q', (snd move), player, True)
 | currentPlayer == blackPlayer &&  newRank == 7 = ('q', (snd move), player, True)
 | otherwise = (name, pos, player, moved)
 where newRank = (fst (snd move)) 

postProcessMovedPawn :: State -> Piece -> Move -> Player -> State
postProcessMovedPawn state piece move player = makeMove state newPawn (snd move)
  where newPawn = promotePawn piece move player


-- Valid moves - rules 
validMove:: State -> Piece -> Move -> Player -> Bool 
validMove state piece move player
 | rook piece = checkRookMove state move player -- Do rook  
 | knight piece = checkKnightMove state move player -- Knight
 | bishop piece = checkBishopMove state move player -- Bishop stuff
 | queen piece = checkQueenMove state move player -- Queen stuff
 | king piece && validMoveKing state piece move player = True
 | pawn piece = checkPawnMove state move player
 | otherwise = False

-- King conditions are more complex - put them in own function
validMoveKing :: State -> Piece -> Move -> Player -> Bool
validMoveKing state piece move player
 | (checkKingMove state move player) || kingSideCast || queenSideCast = True
 where currentBoard = board state
       (kingSideCast, ksRook) = kingSideCastling currentBoard move piece player
       (queenSideCast, qsRook) = queenSideCastling currentBoard move piece player
       newPosition = snd move
       newRookKingSidePos = ((fst (snd move))  , ((snd (snd move)) - 1))
       newRookQueebSidePos = ((fst (snd move))  , ((snd (snd move)) + 1))

-- Rules applicable to the pawn
checkPawnMove:: State -> Move -> Player -> Bool
checkPawnMove state move player
  | diagonalMove move && pieceMovedBefore && pieceOwnedByOtherPlayer && not unoccupiedDestination = True
  | movedForward && movedVertically && (steps == (2,0)) && not pieceMovedBefore && unoccupiedDestination = True
  | movedForward && movedVertically && (steps == (1,0)) && unoccupiedDestination = True
  | otherwise = False
  where destinationPosition = (snd move)
        currentBoard = (board state)
        pieceAtOrigin = (getPieceOnBoard (board state) (fst move))
        pieceAtDestination = (getPieceOnBoard currentBoard destinationPosition)
        unoccupiedDestination = emptyPosition currentBoard destinationPosition
        pieceOwnedByOtherPlayer = pieceOwner (otherPlayer player) pieceAtDestination 
        steps = stepsMoved move
        movedVertically = verticalMove move
        movedForward = forwardMove player move
        pieceMovedBefore = (hasPieceMoved pieceAtOrigin)
-- Rules for the rook
-- Rook is allowed to move horizontally and vertically as long as it is 
-- not obstructed by any other piece 
checkRookMove::State -> Move -> Player -> Bool
checkRookMove state move player
  | horizontalMove move && (emptyDestination || ownedByOtherPlayer )  && (not $ horizontallyObstructed currentBoard move) = True
  | verticalMove move && (emptyDestination || ownedByOtherPlayer ) && (not $ verticallyObstructed currentBoard move) = True 
  | otherwise = False
  where currentBoard = board state
        destinationPosition = (snd move)
        destinationPiece = getPieceOnBoard currentBoard destinationPosition
        emptyDestination = emptyPosition currentBoard destinationPosition
        ownedByOtherPlayer = pieceOwner (otherPlayer player) destinationPiece 
-- Knight movement 
-- Knight can move over pieces
checkKnightMove:: State -> Move -> Player -> Bool 
checkKnightMove state move player = 
  (knightMove move) && (emptyDestination || ownedByOtherPlayer)
 where 
  currentBoard = (board state)
  destinationPosition = (snd move)
  emptyDestination = emptyPosition currentBoard destinationPosition
  ownedByOtherPlayer = pieceOwner (otherPlayer player) (getPieceOnBoard currentBoard destinationPosition) 

-- Bishop movements
-- A bishop moves diagonally
checkBishopMove:: State -> Move -> Player -> Bool
checkBishopMove state move player
  | diagonalMove move && (otherPlayerPiece || emptyDestination) && not obstructed = True
  | otherwise = False
  where
    currentBoard = (board state)
    destinationPosition = (snd move)
    otherPlayerPiece = pieceOwner (otherPlayer player) (getPieceOnBoard currentBoard destinationPosition) 
    emptyDestination = emptyPosition currentBoard destinationPosition
    obstructed = diagonallyObstracted currentBoard move

-- Check queen move 
-- A queen can move just like a bishop or rook
checkQueenMove :: State -> Move -> Player -> Bool
checkQueenMove state move player 
 | diagonalMove move  = checkBishopMove state move player 
 | (horizontalMove move || verticalMove move ) =  checkRookMove state move player
 | otherwise = False

-- King movement
checkKingMove :: State -> Move -> Player -> Bool 
checkKingMove state move player
 | singleStep && horizontalMove move &&
   not (horizontallyObstructed currentBoard move) &&
   (emptyDestination || otherPlayerPiece) &&
   not (kingUnderThreat currentBoard destinationPosition) = True
 | singleStep && verticalMove move && not (verticallyObstructed currentBoard move) &&
    (emptyDestination || otherPlayerPiece) && 
    not (kingUnderThreat currentBoard destinationPosition) = True
 | otherwise = False
 where  destinationPosition = (snd move)
        currentBoard = (board state)
        destinationPiece = getPieceOnBoard currentBoard destinationPosition
        piecePlayed = getPieceOnBoard currentBoard (fst move)
        emptyDestination = emptyPosition currentBoard destinationPosition
        otherPlayerPiece = pieceOwner (otherPlayer player) destinationPiece 
        singleStep = (stepsMoved move == (1,0)) || (stepsMoved move == (0,1))
        twoSteps = (stepsMoved move == (0,2))

-- Castling 
kingSideWhiteRook = (7,7) -- Original position of it hasn't moved
kingSideBlackRook = (0,7) -- Original Position - not moved
queenSideWhiteRook = (7,0) -- Original position
queenSideBlackRook = (0,0)

-- Check if the King castled
kingSideCastling:: Board -> Move -> Piece -> Player -> (Bool, Piece)
kingSideCastling board move piece player
 | kingSide && wplayer && not kingMoved && rook whiteRook && not (horizontallyObstructed board (fst move, kingSideWhiteRook)) = (True , whiteRook)
 | kingSide && bplayer && not kingMoved && rook blackRook  && not (horizontallyObstructed board (fst move, kingSideBlackRook))  = (True, blackRook)
 | otherwise = (False, ('.', (9,9), "", False)) -- Make compiler happy, second part of couple is of no use
 where kingMoved =  (hasPieceMoved piece)
       bplayer = player == whitePlayer
       wplayer = player == blackPlayer
       whiteRook = getPieceOnBoard board kingSideWhiteRook
       blackRook = getPieceOnBoard board kingSideBlackRook
       kingSide = (snd (snd move)) > 3

-- Queen Side castling
queenSideCastling:: Board -> Move -> Piece -> Player -> (Bool, Piece)
queenSideCastling board move piece player
 | queenSide && wplayer && not kingMoved && rook whiteRook  && not (horizontallyObstructed board (fst move, queenSideWhiteRook)) = (True, whiteRook)
 | queenSide && bplayer && not kingMoved && rook blackRook && not (horizontallyObstructed board (fst move, queenSideBlackRook))  = (True, blackRook)
 | otherwise = (False, piece) -- Make compiler happy
 where kingMoved =  (hasPieceMoved piece)
       bplayer = player == whitePlayer
       wplayer = player == blackPlayer
       whiteRook = getPieceOnBoard board queenSideWhiteRook
       blackRook = getPieceOnBoard board queenSideBlackRook
       queenSide = (snd (snd move)) < 3
-- Check if king is under threat
kingUnderThreat :: Board -> Position -> Bool 
kingUnderThreat state position = False
-- Is a piece obstructed horizontally?
horizontallyObstructed :: Board -> Move -> Bool
horizontallyObstructed board ((fromRank, fromFile), (toRank, toFile)) 
 | fromFile < toFile =  False `elem` (map (emptyPosition board) (zip [fromRank, fromRank ..] [(fromFile+1) .. (toFile-1)]))
 | fromFile > toFile =  False `elem` (map (emptyPosition board) (zip [fromRank, fromRank ..] [(fromFile-1), (fromFile-2) .. (toFile+1)]))
 | otherwise = False -- This may never happen

-- Is a piece obstructed vertically?
verticallyObstructed :: Board -> Move -> Bool
verticallyObstructed currentBoard ((fromRank, fromFile), (toRank, toFile)) 
 | fromRank < toRank = False `elem` (map (emptyPosition currentBoard) (zip [(fromRank+1) .. (toRank-1)] [toFile, toFile ..]))
 | fromRank > toRank = False `elem` (map (emptyPosition currentBoard) (zip [(fromRank-1),(fromRank-2)  .. (toRank+1)] [toFile, toFile ..]))
 | otherwise = True

-- Is piece diagonally obstructed?? 
diagonallyObstracted:: Board -> Move -> Bool
diagonallyObstracted board ((fromRank, fromFile), (toRank, toFile))
 | topRight = False `elem` map (emptyPosition board) (zip [fromRank-1, fromRank-2 .. toRank+1] [fromFile+1 .. toFile-1])
 | topLeft = False `elem` map (emptyPosition board) (zip [fromRank-1, fromRank-2 .. toRank+1] [fromFile-1, fromFile-2 .. toFile+1])
 | bottomLeft = False `elem` map (emptyPosition board) (zip  [fromRank+1 .. toRank-1][fromFile-1,fromFile-2 .. toFile+1])
 | bottomRight = False `elem` map (emptyPosition board)  (zip  [fromRank+1 .. toRank-1][fromFile+1 .. toFile-1])
 | otherwise = True
 where topRight = (fromRank > toRank) && (fromFile < toFile)
       bottomLeft = (fromRank < toRank) && (fromFile > toFile)
       topLeft = (fromRank > toRank) && (fromFile > toFile)
       bottomRight = (fromRank < toRank) && (fromFile < toFile)
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

-- Knight move(L o 7) move 
knightMove :: Move -> Bool
knightMove ((fromRank, fromFile), (toRank, toFile))
  | (rankDistance == 2 && fileDistance == 1) || (rankDistance == 1 && fileDistance  == 2) = True
  | otherwise = False
  where rankDistance = abs (toRank - fromRank)
        fileDistance = abs (toFile - fromFile)

-- A knnight attacks in an L shape
-- Relative to a position, it can attack in L or 7 shape
-- These attacking positions are encoded in the list knightAttackingRelativePos 
-- If a night is in any of these positions relative to the position given, then the position is under attack
posUnderAttackByKnight :: Board -> Position -> [Move]
posUnderAttackByKnight board (rank,file) = map (\(_,pos,_,_) -> (pos, (rank,file)))(filter knight (map (getPieceOnBoard board) validPositions))
 where proposedKnightPositions = map (\(r,f) -> (r + rank, f + file)) knightAttackingRelativePos -- calculate attacking knight position relative to the position given
       validPositions = filter checkValidPosition proposedKnightPositions
       knightAttackingRelativePos = [(-2,1), (-2,-1), (2,1), (2,-1),  (-1,2), (-1,-2), (1,2), (1,-2)]
-- Check valid positions 
checkValidPosition :: Position -> Bool
checkValidPosition (rank, file) = (rank >=0 && rank < 8) && (file >= 0 && file < 8)
-- Position under attack diagonally
posUnderAttackDiagonally :: State -> Position -> Player -> [Move]
posUnderAttackDiagonally state position player = validAttackingPieces
 where possibleAttackingPieces = filter (pieceOwner  player) (map (getPieceOnBoard (board state)) (filter checkValidPosition (diagonalPositions position)))
       -- Piece that make valid moves
       validAttackingPieces = filter (\move -> (checkValidPosition (fst move))) $  map (\piece -> if validMove state piece ((getPiecePosition piece), position)  player then ((getPiecePosition piece), position) else ((9,9), position)) possibleAttackingPieces


-- Position under attack horizontally
posUnderAttackHorizontally :: State -> Position -> Player -> [Move]
posUnderAttackHorizontally state  (rank, file) player = validAttackingMoves
  where possibleAttackPieces = filter (pieceOwner player) (map (getPieceOnBoard (board state)) ((zip [rank,rank .. ] [file - 1,file - 2 .. 0]) ++ (zip [rank,rank .. ] [file + 1 .. 7])))
        validAttackingMoves = filter (\move -> checkValidPosition (fst move) && checkValidPosition (snd move)) $ map (\piece -> if (validMove state piece ((getPiecePosition piece), (rank, file))  player) then ((getPiecePosition piece), (rank, file)) else ((9,9),(rank, file))) possibleAttackPieces
        

-- Position under attack vertically
posUnderAttackVertically:: State -> Position -> Player -> [Move]
posUnderAttackVertically state  (rank, file) player = validAttackingMoves
  where possibleAttackPieces = filter (pieceOwner  player) (map (getPieceOnBoard (board state)) ((zip [rank+1 .. 7] [file,file..]) ++ (zip [rank-1, rank-2 .. 0] [file, file .. ])))
        validAttackingMoves =filter (\move -> checkValidPosition (fst move) && checkValidPosition (snd move)) $ map (\piece -> if (validMove state piece ((getPiecePosition piece), (rank, file))  player) then ((getPiecePosition piece), (rank, file)) else ((9,9),(rank, file))) possibleAttackPieces

-- ###Diagonal## ---
-- Diagonal move
diagonalMove :: Move -> Bool
diagonalMove ((fromRank, fromFile), (toRank, toFile)) = 
  abs (fromRank - toRank)  == abs (fromFile - toFile)

-- Generate all possible diagonal pieces relative to a position (with that position excluded)
diagonalPositions:: Position -> [Position]
diagonalPositions (rank, file) =
  topLeft ++ topRight ++ bottomLeft ++ bottomRight
  where topLeft = zip [rank-1 ,(rank-2) .. 0] [file-1, (file-2) .. 0] -- Generate one step away from current position
        topRight = zip [rank-1, (rank-2) .. 0] [(file+1) .. 7]
        bottomLeft = zip [(rank + 1) .. 7] [file-1, (file-2) ..]
        bottomRight = zip [(rank + 1) .. 7] [(file+1)..7]

-- Empty Position?
emptyPosition:: Board -> Position -> Bool
emptyPosition board position = emptyPiece (getPieceOnBoard board position)

-- Positions moved - in both directions 
stepsMoved:: Move -> Step
stepsMoved ((fromRank, fromFile), (toRank, toFile)) = 
  (abs $ fromRank - toRank, abs $ fromFile - toFile)

-- Does a piece belong to a given player? 
pieceOwner::Player -> Piece  -> Bool
pieceOwner player (p,_,_,_) 
 | player == whitePlayer = Data.Char.isUpper p
 | player == blackPlayer = Data.Char.isLower p
 | otherwise = False

-- Is this the first time the piece is moved?
hasPieceMoved:: Piece -> Bool
hasPieceMoved (p,_,_,moved) = moved

-- Driver - enter program here 
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


-- TODO:: Start by testing the attack functions
-- TODO :: Implement en passant 
-- TODO :: check for checkers
-- TODO :: draws
-- TODO -- stalemates