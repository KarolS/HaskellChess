module Uci2Chess where
import Chess
import UCI
import Data.Word
import Data.Maybe
import Data.List
import FEN
columnCharToWord :: Char -> Word8
columnCharToWord c = (toEnum . fromJust)(elemIndex c "abcdefgh")

runUciPosition' :: UciQuery -> Board
runUciPosition' (UciPosition p moves) = foldl' (\b m ->
      doMove (convertMove b m) b
   ) (convertPosition p) moves
runUciPosition :: UciPos -> [UciMove] -> Board
runUciPosition p moves = foldl' (\b m ->
      doMove (convertMove b m) b
   ) (convertPosition p) moves

convertMove :: Board -> UciMove -> Move
convertMove board (UciNormalMove x1 y1 x2 y2) =
   let
      yy1 = (toEnum $ 8-y1)::Word8
      xx1 = (columnCharToWord x1)::Word8
      yy2 = (toEnum $ 8-y2)::Word8
      xx2 = (columnCharToWord x2)::Word8
      piece = getPiece (xx1,yy1) board
   in (piece, (xx1,yy1), (xx2, yy2))
convertMove board (UciPromotionMove x1 y1 x2 y2 p) =
   let
      yy1 = (toEnum $ 8-y1)::Word8
      xx1 = (columnCharToWord x1)::Word8
      yy2 = (toEnum $ 8-y2)::Word8
      xx2 = (columnCharToWord x2)::Word8
      piece = case (whoseTurn board, p) of
         (True,  'q') -> wQ
         (True,  'r') -> wR
         (True,  'b') -> wB
         (True,  'n') -> wN
         (False, 'q') -> bQ
         (False, 'r') -> bR
         (False, 'b') -> bB
         (False, 'n') -> bN
         _            -> 0
   in (piece, (xx1,yy1), (xx2, yy2))
convertMove board UciNullmove = error "NULL MOVE"

convertPosition :: UciPos -> Board
convertPosition UciStartpos = startingBoard
convertPosition (UciFen s1 s2 s3 s4 s5 s6) = splitFenToBoard s1 s2 s3 s4 s5 s6

moveToUci :: Move -> Board -> String
moveToUci (piece, (x1,y1), (x2,y2)) board =
   ("abcdefgh" !! fromEnum x1) :
   ("87654321" !! fromEnum y1) :
   ("abcdefgh" !! fromEnum x2) :
   ("87654321" !! fromEnum y2) :
   if piece == getPiece (x1,y1) board
      then ""
      else (" pnbrqk    pnbrqk" !! fromEnum piece) : ""

uciMovesToPgn string = helper 1 startingBoard (parseUciMoves string)
   where
      helper i board [] = ""
      helper i board (ply:[]) = show i ++ ". " ++ showMoveInContext board (convertMove board ply)
      helper i board (wply:bply:restOfMoves) =
         let
            cwply   = convertMove board  wply
            board'  = doMove cwply board
            cbply   = convertMove board' bply
            board'' = doMove cbply board'
         in
            show i ++ ". " ++
            showMoveInContext board  cwply ++ " " ++
            showMoveInContext board' cbply ++ " " ++
            helper (i+1) board'' restOfMoves
