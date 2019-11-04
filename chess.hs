import qualified Data.Char
import Representation
import Validation
-- check for draws by insufficient material
whiteSquares =  (zip [0,2..7] [0,0..]) ++ (zip [1,3..7] [1,1..]) ++ (zip [0,2..7] [2,2..]) ++ (zip [1,3..7] [3,3..]) ++ (zip [0,2..7] [4,4..])
                 ++ (zip [1,3..7] [5,5..]) ++ (zip [0,2..7] [6,6..])  ++ (zip [1,3..7] [7,7..])
-- k-k
-- k&k-b
-- k&k-n
-- k-b&k-b bishops reside on same color
-- TODO::  Very messy implementation -  Refactor this !!
drawByInsufficientMaterial:: Board -> Bool
drawByInsufficientMaterial board
 | length blackPlayerPieces == 1 && length whitePlayerPieces == 1 && 'K' `elem` whitePlayerPieces &&  'k' `elem` blackPlayerPieces = True
 | length blackPlayerPieces == 2 && length whitePlayerPieces == 1 && 'K' `elem` whitePlayerPieces && 'k' `elem` blackPlayerPieces && 'n' `elem` blackPlayerPieces = True
 | length blackPlayerPieces == 1 && length whitePlayerPieces == 2 && 'K' `elem` whitePlayerPieces && 'k' `elem` blackPlayerPieces && 'N' `elem` whitePlayerPieces = True
 | length blackPlayerPieces == 2 && length whitePlayerPieces == 1 && 'K' `elem` whitePlayerPieces && 'k' `elem` blackPlayerPieces && 'b' `elem` blackPlayerPieces = True
 | length blackPlayerPieces == 1 && length whitePlayerPieces == 2 && 'K' `elem` whitePlayerPieces && 'k' `elem` blackPlayerPieces && 'B' `elem` whitePlayerPieces = True
 | length blackPlayerPieces == 2 && length whitePlayerPieces == 2 && (wbishop !! 0) `elem` whiteSquares &&  (bbishop !! 0) `elem` whiteSquares = True
 | length blackPlayerPieces == 2 && length whitePlayerPieces == 2 && not ((wbishop !! 0) `elem` whiteSquares) &&  not ((bbishop !! 0) `elem` whiteSquares) = True 
 | otherwise = False
 where blackPlayerPieces = map getPieceName (getPlayerPieces board blackPlayer)
       whitePlayerPieces = map getPieceName (getPlayerPieces board whitePlayer)
       bbishop = map getPiecePosition (filter bishop (getPlayerPieces board blackPlayer))
       wbishop = map getPiecePosition (filter bishop (getPlayerPieces board whitePlayer))
       blackPlayerPos = map getPiecePosition (getPlayerPieces board blackPlayer)
       whitePlayerPos = map getPiecePosition (getPlayerPieces board whitePlayer)
-- Get player pieces
getPlayerPieces :: Board -> Player -> [Piece]
getPlayerPieces board player = flatBoard
  where flatBoard = filter (pieceOwner player) (concat (map (\(rank, pieces) -> pieces) board))
-- check if a player is in a stalemate
checkStalemate :: State -> Player -> Bool
checkStalemate state player = (length (map (hasValidMoves state player) playerPieces)) == 0
 where playerPieces = getPlayerPieces (board state) player

-- Has valid moves
hasValidMoves:: State -> Player -> Piece -> Bool
hasValidMoves state player piece
  | simulateMoves state piece position player positions = True
  | otherwise =  False
  where position = getPiecePosition piece
        positions = generatePositions piece

-- Simulate movements of the pieces
simulateMoves :: State -> Piece -> Position -> Player -> [Position] -> Bool
simulateMoves _ _ _ _ [] = False
simulateMoves state piece position player (pos:positions)
  | validMove state piece (position, pos) player =  
    if (kingUnderThreat (makeMove state piece pos) player) then (simulateMoves state piece position player positions) else True
    -- if the move is valid, it must not leave this player's king under attack
  | otherwise = simulateMoves state piece position player positions
-- Control the game from here
step :: State -> Command -> (Message, Maybe State)
step state command
 | pawn piece  && isValidMove = validateSuccess state (postProcessMovedPawn state  piece moveToBeMade currentPlayer) currentPlayer
 | king piece && isValidMove = validateSuccess state (makeMove state piece newPosition) currentPlayer
 | isValidMove = validateSuccess state (makeMove state piece newPosition) currentPlayer
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
validateSuccess :: State -> State  -> Player -> (Message, Maybe State)
validateSuccess oldState newState currentPlayer
 | drawByInsufficientMaterial (board newState) = ("Draw by insufficient material", Nothing)
 | kingUnderThreat newState currentPlayer = ("Invalid move, your king will be in check!! \n" ++ show currentPlayer ++ ", play again", Just oldState)
 | kingUnderThreat newState (otherPlayer currentPlayer) && checkStalemate newState (otherPlayer currentPlayer) = ("Game over!! \n Checkers!!", Nothing)
 | checkStalemate newState (otherPlayer currentPlayer)  = ("Game over !! \n" ++ show (otherPlayer currentPlayer) ++ " has no legal moves", Nothing)
 | checkStalemate newState currentPlayer  = ("Game over !! \n" ++ show  currentPlayer ++ " has no legal moves", Nothing)
 | otherwise =  ("🎊🎊🎊 Nice Move!!\n" ++ show (otherPlayer currentPlayer) ++ ", it is your turn",Just $ newState)
-- Failure message 
showError :: State -> (Message, Maybe State)
showError state = ("Invalid move/command, try again", Just state)

-- Helper function for promoting pawn
-- Promote a pawn to a queen if it reaches the oppisite end of the board
promotePawn :: Piece -> Move -> Player ->  Piece
promotePawn (name, pos, player, moved) move currentPlayer 
 | currentPlayer == whitePlayer &&  newRank == 0 = ('Q', (snd move), player, True)
 | currentPlayer == blackPlayer &&  newRank == 7 = ('q', (snd move), player, True)
 | otherwise = (name, pos, player, moved)
 where newRank = (fst (snd move)) 

postProcessMovedPawn :: State -> Piece -> Move -> Player -> State
postProcessMovedPawn state piece move player
  | not pawnMoved && length attackingPieces > 0 =  makeMove (makeMove state (getPieceOnBoard (board state) (fst move)) enPassantPos) (getPieceOnBoard (board state) (fst (attackingPieces !! 0))) enPassantPos
  | otherwise = (makeMove state newPawn newPosition)
  where newPawn = promotePawn piece move player
        newPosition = (snd move)
        pawnMoved = hasPieceMoved piece
        (enPassantPos, attackingPieces) = enPassantPawnCapture state move player

-- Driver - enter program here
main :: IO ()
main = loop $ Just state0
  where loop Nothing = return()
        loop (Just s) =
          do
            putStrLn (if s == state0 then show s ++ "\n\n " ++whitePlayer++ ",it is your turn" else "")
            c <- getLine
            let (m, ms) = step s c
            putStrLn $ show ms
            putStrLn m
            loop ms
