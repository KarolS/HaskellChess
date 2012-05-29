#include "common.h"

module FEN(fenToBoard,splitFenToBoard) where
import Chess
import Data.Word
import Control.Applicative
import Data.Maybe
import Data.List

splitFenToBoard :: String -> String -> String -> String -> String -> String -> Board
splitFenToBoard board turn castling ep pliesToDraw  moveNo =
   let b = boardFromListOfStrings $ fs' board
   in b{
      canWhiteCastleWest = 'Q' `elem` castling || 'A' `elem` castling,
      canWhiteCastleEast = 'K' `elem` castling || 'H' `elem` castling,
      canBlackCastleWest = 'q' `elem` castling || 'a' `elem` castling,
      canBlackCastleEast = 'k' `elem` castling || 'h' `elem` castling,
      canDoEnpassantAt = if length ep < 2
         then Nothing
         else ( (\x y -> (x,y))
         <$> fmap toEnum (elemIndex (ep!!0) "abcdefgh")
         <*> fmap toEnum (elemIndex (ep!!1) "87654321")
         ),
      whoseTurn = if head turn `elem` "Ww" then White else Black
   }
 --TODO
fenToBoard fen =
   let
      [board, turn, castling, ep, pliesToDraw, moveNo] = words fen
   in splitFenToBoard board  turn  castling  ep  pliesToDraw  moveNo

fs' = fs "" []
fs :: String -> [String] -> String -> [String]
fs a as      ""  = reverse $ a:as
fs a as ('/':xs) = fs "" (reverse a:as) xs
fs a as ('1':xs) = fs (' ':a) as xs
fs a as ('2':xs) = fs ("  "++a) as xs
fs a as ('3':xs) = fs ("   "++a) as xs
fs a as ('4':xs) = fs ("    "++a) as xs
fs a as ('5':xs) = fs ("     "++a) as xs
fs a as ('6':xs) = fs ("      "++a) as xs
fs a as ('7':xs) = fs ("       "++a) as xs
fs a as ('8':xs) = fs ("        "++a) as xs
fs a as ( x :xs) = fs (x:a) as xs
