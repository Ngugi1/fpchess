module Validation where
import Representation
import Text.Regex.Posix
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

-- Remove a piece from a rank by replacing it with an empty piece
removePiece:: Position -> (Rank, [Piece]) -> (Rank, [Piece])
removePiece position pieces =
  replacePiece position ('.',position, noplayer, True) pieces
-- Place a piece or replace at the position with the new piece 
replacePiece:: Position -> Piece -> (Rank, [Piece]) -> (Rank, [Piece])
replacePiece (rank, file) (pname,_,player,_) (rn, pieces) =
  (rn, map filterPiece pieces)
  where filterPiece (n,(r, f),p,m) = 
         if (rank == r && f == file)
         then (pname, (rank,file),player,True)
         else (n,(r, f),p,m)
-- Make a move
makeMove:: State -> Piece -> To -> State
makeMove state (name, position, player, moved) to =
   State  (map (replacePiece to (name, to, player, True)) (map (removePiece position) (board state))) (otherPlayer player)



-- Valid moves - dispatch based on the piece 
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
-- ####  King movement ###
checkKingMove :: State -> Move -> Player -> Bool 
checkKingMove state move player
 | singleStep && horizontalMove move &&
   not (horizontallyObstructed currentBoard move) &&
   (emptyDestination || otherPlayerPiece) = True
 | singleStep && verticalMove move && not (verticallyObstructed currentBoard move) &&
    (emptyDestination || otherPlayerPiece) = True
 | otherwise = False
 where  destinationPosition = (snd move)
        currentBoard = (board state)
        destinationPiece = getPieceOnBoard currentBoard destinationPosition
        piecePlayed = getPieceOnBoard currentBoard (fst move)
        emptyDestination = emptyPosition currentBoard destinationPosition
        otherPlayerPiece = pieceOwner (otherPlayer player) destinationPiece 
        singleStep = (stepsMoved move == (1,0)) || (stepsMoved move == (0,1))
        twoSteps = (stepsMoved move == (0,2))

-- Castling - original rook positions
kingSideWhiteRook = (7,7) -- Original position if it hasn't moved
kingSideBlackRook = (0,7) -- Original Position - not moved
queenSideWhiteRook = (7,0) -- Original position
queenSideBlackRook = (0,0)

-- Check if the King castled
kingSideCastling:: Board -> Move -> Piece -> Player -> (Bool, Piece)
kingSideCastling board move piece player
 | kingSide && wplayer && not kingMoved && rook whiteRook && not (horizontallyObstructed board (fst move, kingSideWhiteRook)) = (True , whiteRook)
 | kingSide && bplayer && not kingMoved && rook blackRook  && not (horizontallyObstructed board (fst move, kingSideBlackRook))  = (True, blackRook)
 | otherwise = (False, piece)
 where kingMoved =  (hasPieceMoved piece)
       bplayer = player == blackPlayer
       wplayer = player == whitePlayer
       whiteRook = getPieceOnBoard board kingSideWhiteRook
       blackRook = getPieceOnBoard board kingSideBlackRook
       kingSide = (snd (snd move)) > 3
-- Queen Side castling
queenSideCastling:: Board -> Move -> Piece -> Player -> (Bool, Piece)
queenSideCastling board move piece player
 | queenSide && wplayer && not kingMoved && rook whiteRook  && not (horizontallyObstructed board (fst move, queenSideWhiteRook)) = (True, whiteRook)
 | queenSide && bplayer && not kingMoved && rook blackRook && not (horizontallyObstructed board (fst move, queenSideBlackRook))  = (True, blackRook)
 | otherwise = (False, piece)
 where kingMoved =  (hasPieceMoved piece)
       bplayer = player == whitePlayer
       wplayer = player == blackPlayer
       whiteRook = getPieceOnBoard board queenSideWhiteRook
       blackRook = getPieceOnBoard board queenSideBlackRook
       queenSide = (snd (snd move)) < 3

-- Check if king is under threat - being attacked
kingUnderThreat :: State -> Player -> Bool
kingUnderThreat state player = (length (positionUnderAttack state (getPiecePosition playersKing) player)) > 0
  where playersKing = (filter (pieceOwner player) (filter king (concat (map (\(rank, pieces) -> pieces) (board state))))) !! 0

-- Pawn rules
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
        (enPassantPos, enPassantAttacks) = enPassantPawnCapture state move player

-- Is it an enpassant ? 
enPassantPawnCapture :: State -> Move -> Player -> (Position, [Move])
enPassantPawnCapture state ((fRank, fFile),to) player
 | player == whitePlayer && (steps == (2,0)) = blackAttackingPieces
 | player == blackPlayer && (steps == (2,0)) = whiteAttackingPieces
 | otherwise = ((9,9), [])
  where whiteEnPassantPos = (fRank-1, fFile)
        blackEnPassantPos = (fRank+1, fFile)
        steps = (stepsMoved((fRank, fFile),to))
        blackAttackingPieces = (whiteEnPassantPos, (positionUnderAttack state whiteEnPassantPos (otherPlayer player)))
        whiteAttackingPieces = (whiteEnPassantPos, (positionUnderAttack state blackEnPassantPos (otherPlayer player)))


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
