#include "common.h"

module Ai (alphaBeta, minmax)where
import Chess
import Data.Array.Base

boundedMax alpha beta [] = alpha
boundedMax alpha beta (x:xs) =
   if alpha>=beta
   then alpha
   else
      let m = max alpha x
      in boundedMax m beta xs
boundedSafeEval alpha beta oldPosition [] =
    if isKingInCheck oldPosition
        then if whoseTurn oldPosition == White then -10000000 else 10000000
        else 0
boundedSafeEval alpha beta _ moves =
    negate $ boundedMax (-beta) (-alpha) moves

safeEval oldPosition [] =
    if isKingInCheck oldPosition
        then if (whoseTurn oldPosition == White) then -10000000 else 10000000
        else 0
safeEval _ moves =
    negate $ foldr1 max moves

boundedEvaluatePosition::Double->Double->Int->Board->Double
boundedEvaluatePosition a b 0 position =
    let
      value = rawEvaluate position
      isWhite = whoseTurn position == White
    in if isWhite then value else -value

boundedEvaluatePosition a b depth position =
   boundedSafeEval a b position [
      boundedEvaluatePosition a b (depth-1) (doMove move position)
      | move <- getAllLegalMoves position
      ]
evaluatePosition 0 position  =
    let
      value = rawEvaluate position
      isWhite = whoseTurn position == White
    in if isWhite then value else -value
evaluatePosition depth position  =
   safeEval position [
      evaluatePosition (depth-1) (doMove move position)
      | move <- getAllLegalMoves position ]

minmax depth isWhite position =
    let
        moves = getAllLegalMoves position
        variants = [(evaluatePosition depth (doMove move position), move) | move<-moves]
    in if variants==[]
        then Nothing
        else Just $ snd $ foldr1 max variants

alphaBeta depth isWhite position =
    let
        moves = getAllLegalMoves position
        variants = [(boundedEvaluatePosition (-10000001) 10000000 depth (doMove move position), move) | move<-moves]
    in if variants==[]
        then Nothing
        else Just $ snd $ foldr1 max variants

pieceValue :: Piece -> Double
--pieceValue c = va ! fromEnum c
--    where va = listArray (0,19::Int) [ 0.0, 1.0, 3.0, 3.0, 5.0, 9.5, 1000000.0, 0.0, 0.0, 0.0, 0.0,-1.0,-3.0,-3.0,-5.0,-9.5,-1000000.0, 0.0, 0.0, 0.0 ]
pieceValue c = va !! fromEnum c
    where va = [ 0.0, 1.0, 3.0, 3.0, 5.0, 9.5, 1000000.0, 0.0, 0.0, 0.0, 0.0,-1.0,-3.0,-3.0,-5.0,-9.5,-1000000.0, 0.0, 0.0, 0.0 ]

-- TODO: szachmat i remis
rawEvaluate b@Board{pieceArray=a} =
    sum [pieceValue(a!(x,y)) | x<-[0..7], y<-[0..7]] +
    fromIntegral(length(getAllLegalMovesForWhite b))/60.0 -
    fromIntegral(length(getAllLegalMovesForBlack b))/60.0
