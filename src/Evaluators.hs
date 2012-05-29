#include "common.h"

module Evaluators where
import Chess
import Utils
import Data.Array.Base

evaluateBoard :: Board -> (Double, Double)
evaluateBoard board@Board{pieceArray=a} =
   let
      wMoves' = getAllLegalMoves'ForWhite board
      bMoves' = getAllLegalMoves'ForBlack board
      lwMoves' = fromIntegral $! length wMoves'
      lbMoves' = fromIntegral $! length bMoves'
      checkBonus = if isKingLegallyInCheck board then 1.0 else 0.0
      lowMovesBonus = negIf (whoseTurn board == White) $! (lbMoves'-lwMoves')
      {-attackBonusFunction moveList  = map (\(piece, from, to) ->
         let
            attVal = abs $ pieceValue piece
            defVal = abs $ pieceValue $ a!to
         in if attVal>defVal then defVal/2.0 else defVal
         ) $! moveList-}
      smartAttackFunction moveList  = map (\(piece, from, to) ->
         let
            attVal = abs $! pieceValue $! piece
            defVal = abs $! pieceValue $! a!to
         in if attVal>defVal then 0 else defVal
         ) $! moveList
      --whiteAttackBonus = sum $! attackBonusFunction wMoves'
      --blackAttackBonus = sum $! attackBonusFunction bMoves'
      whiteSmartAttackBonus = sum $! smartAttackFunction wMoves'
      blackSmartAttackBonus = sum $! smartAttackFunction bMoves'
      rawInteresting =
         checkBonus*10.0 + lowMovesBonus*0.05 +
         whiteSmartAttackBonus*2.0 + blackSmartAttackBonus*2.0
      rawEvaluate =
         sum [pieceValue(a!(x,y)) | x<-[0..7], y<-[0..7]] +
         if isKingLegallyInCheck board
         then if   whoseTurn board == White
              then
                  let
                     realMoves = getAllLegalMovesForWhite board
                  in
                     case realMoves of
                        [] -> -1000000.0
                        _ ->
                           sqrt(sqrt lwMoves') / 10.0 - sqrt lbMoves' / 10.0
              else
                  let
                     realMoves = getAllLegalMovesForBlack board
                  in
                     case realMoves of
                        [] -> 1000000.0
                        _ ->
                           sqrt lwMoves' / 10.0 - sqrt(sqrt lbMoves') / 10.0
         else
            sqrt lwMoves' / 10.0 - sqrt lbMoves' / 10.0 +
            whiteSmartAttackBonus/10.0 - blackSmartAttackBonus/10.0
   in
      (rawInteresting, rawEvaluate)


pieceValue :: Piece -> Double
pieceValue c = pieceValueArray ! c

pieceValueArray :: UArray Piece Double
pieceValueArray = listArray (0,19) $! [
   0.0, 1.0, 3.0, 3.0, 5.0,
   09.5, 1000000.0, 0.0, 0.0, 0.0,
   0.0,-1.0,-3.0,-3.0,-5.0,
   -9.5,-1000000.0, 0.0, 0.0, 0.0 ]

-- TODO: szachmat i remis


