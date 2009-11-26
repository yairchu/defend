{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module GameLogic
  ( Move(..), PartialData(..), visibleSquares
  ) where

import Chess

import Control.Monad (guard)
import Data.List (group, partition, sort)

data Move = Move
  { moveSrc :: BoardPos
  , movePlayer :: PieceSide
  , moveIter :: Integer
  , moveDst :: BoardPos
  }
  deriving (Read, Show)

class PartialData a b where
  getPartial :: b -> a

instance PartialData a a where
  getPartial = id

instance PartialData a b => PartialData a (c -> b) where
  getPartial x = getPartial (x undefined)

sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort

visibleSquares :: PieceSide -> Board -> [BoardPos]
visibleSquares side board =
  sortNub $ (goodPieces >>= sees) ++ (evilPieces >>= threats)
  where
    (goodPieces, evilPieces) =
      partition ((== side) . pieceSide) (boardPieces board)
    theKings = filter ((== King) . pieceType) goodPieces
    [theKing] = theKings
    sees p = piecePos p : map fst (possibleMoves board p)
    threats p = do
      guard . not . null $ theKings
      guard $ piecePos theKing `elem` sees p
      return $ piecePos p

