module GameLogic (
  visibleSquares
  ) where

import Chess

import Control.Monad (guard)
import Data.List (group, partition, sort)

sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort

visibleSquares :: PieceSide -> Board -> [BoardPos]
visibleSquares side board =
  sortNub $ (sees =<< goodPieces) ++ (threats =<< evilPieces)
  where
    (goodPieces, evilPieces) =
      partition ((== side) . pieceSide) (boardPieces board)
    [theKing] = filter ((== King) . pieceType) goodPieces
    sees p = piecePos p : map fst (possibleMoves board p)
    threats p = do
      guard $ piecePos theKing `elem` sees p
      return $ piecePos p

