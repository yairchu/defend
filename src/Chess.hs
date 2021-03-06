module Chess (
  PieceType(..), BoardPos, PieceSide(..), Piece(..), Board(..),
  chessStart, pieceAt, possibleMoves
  ) where

import Control.Monad (guard)
import Data.Foldable (Foldable, all, any, toList)
import Prelude hiding (all, any, null)

data PieceType =
  Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Read, Show)

type BoardPos = (Integer, Integer)

data PieceSide = Black | White
  deriving (Eq, Read, Show)

data Piece = Piece {
  pieceSide :: PieceSide,
  pieceType :: PieceType,
  piecePos :: BoardPos,
  pieceMoved :: Bool
}

data Board = Board {
  boardPieces :: [Piece],
  boardLastMove :: Maybe (Piece, BoardPos)
}

chessStart :: Board
chessStart =
  Board {
    boardPieces =
      concat (zipWith headRowItems [0..7] headRowTypes) ++
      [Piece side Pawn (x, y) False |
      x <- [0..7], (side, y) <- [(White, 1), (Black, 6)]],
    boardLastMove = Nothing
  }
  where
    headRowTypes = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    headRowItems x t = do
      (col, row) <- [(White, 0), (Black, 7)]
      return $ Piece col t (x, row) False

addPos :: BoardPos -> BoardPos -> BoardPos
addPos (xa, ya) (xb, yb) = (xa+xb, ya+yb)

rays :: PieceType -> [[BoardPos]]
rays Knight = do
  a <- [1, -1]
  b <- [2, -2]
  [[(a, b)], [(b, a)]]
rays Bishop = do
  dx <- [1, -1]
  dy <- [1, -1]
  return $ iterate (addPos (dx, dy)) (dx, dy)
rays Rook = do
  (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
  return $ iterate (addPos (dx, dy)) (dx, dy)
rays Queen = rays Bishop ++ rays Rook
rays King = map (take 1) $ rays Queen
rays Pawn = []

pieceAt :: Board -> BoardPos -> Maybe Piece
pieceAt board pos =
  case filter ((== pos) . piecePos) (boardPieces board) of
    [] -> Nothing
    (x : _) -> Just x

takeUntilIncluding :: (a -> Bool) -> [a] -> [a]
takeUntilIncluding _ [] = []
takeUntilIncluding func (x : xs)
  | func x = [x]
  | otherwise = x : takeUntilIncluding func xs

null :: Foldable t => t a -> Bool
null = all (const False)

possibleMoves :: Board -> Piece -> [(BoardPos, Board)]
possibleMoves board piece =
  simpleMoves ++ otherMoves (pieceType piece)
  where
    simpleMoves = do
      relRay <- rays (pieceType piece)
      dst <-
        takeUntilIncluding (not . null . pieceAt board) .
        takeWhile notBlocked $
        map (addPos src) relRay
      return $ simpleMove dst
    src@(srcX, _) = piecePos piece
    move updPiece clearPos dst =
      (dst, newBoard)
      where
        newBoard =
          Board {
            boardPieces =
              newPieceState :
              filter (not . (`elem` [src, clearPos]) . piecePos)
              (boardPieces board),
            boardLastMove = Just (newPieceState, src)
          }
        newPieceState =
          updPiece { piecePos = dst, pieceMoved = True }
    simpleMove dst = move piece dst dst
    inBoard (x, y) = 0 <= x && x < 8 && 0 <= y && y < 8
    isOtherSide = (/= pieceSide piece) . pieceSide
    notBlocked pos =
      inBoard pos &&
      all isOtherSide (pieceAt board pos)
    promotionRow = pawnStartRow + 6 * forward
    pawnMove dst@(_, dy)
      | dy /= promotionRow = simpleMove dst
      | otherwise = move (piece { pieceType = Queen }) dst dst
    otherMoves Pawn =
      (enPassant (boardLastMove board) ++) $
      map pawnMove .
      filter inBoard $
      moveForward ++
      filter (any isOtherSide . pieceAt board)
        [(sx-1, sy+forward), (sx+1, sy+forward)] ++
      do
        guard $ sy == pawnStartRow
        guard . not $ null moveForward
        guard . null $ pieceAt board sprintDst
        return sprintDst
      where
        moveForward = filter (null . pieceAt board) [(sx, sy+forward)]
        sprintDst = (sx, sy+forward*2)
        enPassant Nothing = []
        enPassant (Just (lpiece, (mx, my)))
          | py /= sy = []
          | abs (mx-sx) /= 1 = []
          | abs (py-my) /= 2 = []
          | pieceType lpiece /= Pawn = []
          | pieceSide lpiece == pieceSide piece = []
          | not (null (pieceAt board dst)) = []
          | otherwise = [move piece prevPos dst]
          where
            dst = (mx, sy+forward)
            prevPos@(_, py) = piecePos lpiece
    otherMoves King = do
      guard . not $ pieceMoved piece
      (rookX, direction) <- [(0, -1), (7, 1)]
      let
        dangerZone =
          map fst $
          concatMap (possibleMoves board) dangers
        dangers = filter isDanger $ boardPieces board
        isDanger p =
          pieceSide p /= pieceSide piece &&
          (pieceMoved p || pieceType p /= King)
        rookDstX = srcX + direction
        kingDstX = srcX + 2*direction
        putInRow x = (x, sy)
        clearPath = map putInRow [srcX + direction, srcX + direction*2 .. rookX - direction]
        safePath = map putInRow [srcX, srcX + direction]
        rookPos = (rookX, sy)
        kingDst = (kingDstX, sy)
        newKingState =
          piece { piecePos = kingDst, pieceMoved = True }
      rook <- toList $ pieceAt board rookPos
      guard $ Rook == pieceType rook
      guard . not . pieceMoved $ rook
      guard . all (null . pieceAt board) $ clearPath
      guard . all (`notElem` dangerZone) $ safePath
      let
        newRookState =
          rook { piecePos = (rookDstX, sy), pieceMoved = True }
        newBoard =
          Board {
            boardPieces =
              [newKingState, newRookState] ++
              filter (not . (`elem` [src, rookPos]) . piecePos)
              (boardPieces board),
            boardLastMove = Just (newKingState, src)
          }
      return (kingDst, newBoard)
    otherMoves _ = []
    (forward, pawnStartRow)
      | pieceSide piece == White = (1, 1)
      | otherwise = (-1, 6)
    (sx, sy) = src

