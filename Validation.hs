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

-- Diagonal movement
diagonalMove :: Move -> Bool
diagonalMove ((fromRank, fromFile), (toRank, toFile)) = 
  abs (fromRank - toRank)  == abs (fromFile - toFile)

-- Knight move(L o 7) move 
knightMove :: Move -> Bool
knightMove ((fromRank, fromFile), (toRank, toFile))
  | (rankDistance == 2 && fileDistance == 1) || (rankDistance == 1 && fileDistance  == 2) = True
  | otherwise = False
  where rankDistance = abs (toRank - fromRank)
        fileDistance = abs (toFile - fromFile)
-- Positions moved - in both directions 
stepsMoved:: Move -> Step
stepsMoved ((fromRank, fromFile), (toRank, toFile)) = 
  (abs $ fromRank - toRank, abs $ fromFile - toFile)

-- ########## check for obstruction
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
--- ########### Check if a position is under attack
  -- Position under attack 
positionUnderAttack:: State -> Position -> Player -> [Move]
positionUnderAttack state position player = posUnderAttackByKnight state position player ++  
                                            posUnderAttackDiagonally  state position player ++
                                            posUnderAttackHorizontally state position player ++
                                            posUnderAttackVertically state position player

-- A knight attacks in an L or 7 shape
-- Relative to a position, it can attack in L or 7 shape
-- These attacking positions are encoded in the list knightAttackingRelativePos 
-- If a night is in any of these positions relative to the position given, then the position is under attack
posUnderAttackByKnight :: State -> Position -> Player -> [Move]
posUnderAttackByKnight state (rank,file) player = map (\(_,pos,_,_) -> (pos, (rank,file))) (filter knight  validAttackingPieces)
 where proposedKnightPositions = map (\(r,f) -> (r + rank, f + file)) knightAttackingRelativePos -- calculate attacking knight position relative to the position given
       validAttackingPieces = filter (pieceOwner player) (map (getPieceOnBoard (board state)) (filter checkValidPosition proposedKnightPositions))
       knightAttackingRelativePos = [(-2,1), (-2,-1), (2,1), (2,-1),  (-1,2), (-1,-2), (1,2), (1,-2)]
-- Position under attack diagonally
posUnderAttackDiagonally :: State -> Position -> Player -> [Move]
posUnderAttackDiagonally state position player = validAttackingPieces
 where possibleAttackingPieces = filter (pieceOwner  player) (map (getPieceOnBoard (board state)) (diagonalPositions position))
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

-- ###  Generate positions
-- Generate all possible diagonal pieces relative to a position (with that position excluded)
diagonalPositions:: Position -> [Position]
diagonalPositions (rank, file) =
   filter checkValidPosition (topLeft ++ topRight ++ bottomLeft ++ bottomRight)
  where topLeft = zip [rank-1 ,(rank-2) .. 0] [file-1, (file-2) .. 0] -- Generate one step away from current position
        topRight = zip [rank-1, (rank-2) .. 0] [(file+1) .. 7]
        bottomLeft = zip [(rank + 1) .. 7] [file-1, (file-2) ..]
        bottomRight = zip [(rank + 1) .. 7] [(file+1)..7]

-- Generate horizontal positions -- useful for the rook and queen
horizontalPositions :: Position -> [Position]
horizontalPositions (rank, file) = filter checkValidPosition (zip [rank,rank..] [file-1,file-2 ..0]) ++ (zip [rank,rank..] [file+1..7])

-- Generate vertical positions -- useful for rook and queen
verticalPositions :: Position -> [Position]
verticalPositions (rank, file) = filter checkValidPosition (zip [rank-1,rank-2 .. 0][file,file..]) ++ (zip [rank+1 .. 7][file,file..])

-- pawn positions
pawnPositions :: Position -> [Position]
pawnPositions (rank, file) = filter checkValidPosition [(rank+1, file), (rank+1,file+1), (rank+2, file)]

-- Generate knight posittions
knightPositions :: Position -> [Position]
knightPositions (rank,file) = filter checkValidPosition (map (\(r,f) -> (r+rank, f+file)) [(-2,1), (-2,-1), (2,1), (2,-1),  (-1,2), (-1,-2), (1,2), (1,-2)])

-- Generate king positions 
kingPositions :: Position -> [Position]
kingPositions (rank, file) = filter checkValidPosition [(rank+1,file), (rank-1, file), (rank+1,file), (rank-1, file),(rank, file+2), (rank, file-2) ]

-- generate positions 
generatePositions :: Piece -> [Position]
generatePositions piece
 | pawn piece = pawnPositions position
 | rook piece = (verticalPositions position) ++ (horizontalPositions position)
 | knight piece = kingPositions position
 | bishop piece =  diagonalPositions position
 | queen piece = (verticalPositions position) ++ (horizontalPositions position) ++ (horizontalPositions position)
 | king piece = kingPositions position
 | otherwise = []
 where position = getPiecePosition piece

