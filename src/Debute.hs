
module Debute where
import Data.Array.Base
import Chess
evaluateBoardInDebute  :: Board -> (Double, Double)
evaluateBoardInDebute board@Board{pieceArray=a} =
   let
      rawEvaluate =
         sum [debutePieceValue(a!(x,y)) | x<-[0..7], y<-[0..7]]
   in
      (0,0)

debutePieceValue _ = 0.0
