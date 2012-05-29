#include "common.h"

module Chess(
   Board(..),
   Piece,
   Coord,
   Move,
   Player(..),
   opponent,
   showCoord,
   showMove,
   showMoveInContext,
   emptyBoard,
   startingBoard,
   getPiece,
   putPiece,
   boardFromListOfStrings,
   doMove,
   getAllLegalMoves,
   getAllLegalMovesForWhite,
   getAllLegalMovesForBlack,
   getAllLegalMoves'ForWhite,
   getAllLegalMoves'ForBlack,
   hasPiece,
   bK,bQ,bR,bB,bN,bP,
   wK,wQ,wR,wB,wN,wP,
   isKingInCheck,
   isKingLegallyInCheck,
   getUniquePiecePosition
) where

import Data.Array.Base
import Data.Array.IArray
import Data.Word
import Data.Maybe
import Data.List
import Data.Char

data Board = Board {
        pieceArray :: UArray (Word8, Word8) Word8,
        isEnded :: Bool,
        whoseTurn :: Player,
        canWhiteCastleWest :: Bool,
        canWhiteCastleEast :: Bool,
        canBlackCastleWest :: Bool,
        canBlackCastleEast :: Bool,
        canDoEnpassantAt :: Maybe Coord,
        whiteKingAt :: Coord,
        blackKingAt :: Coord
        -- TODO: more
    }

instance Show Board where
    show Board{pieceArray=a} = '\n': unlines [
            [
                pieceToChar $ a!(x,y) | x<-[0..7]
            ] | y<-[0..7]
        ]
type Piece = Word8
type Coord = (Word8, Word8)
type Move = (Piece, Coord, Coord)
type Фигура = Word8
type Коорд = (Word8, Word8)
type Ход = (Фигура, Коорд, Коорд)
type Доска = Board

--data Player = White | Black deriving (Eq,Ord,Show)

--opponent Black = White
--opponent White = Black

isBlack i = i>10
isEmpty i = i==0
isWhite i = i<10 && i>0
isBlackKillable i = i>10 && i<16
isWhiteKillable i = i<6 && i>0

bP = 11::Word8
bR = 14::Word8
bN = 12::Word8
bB = 13::Word8
bQ = 15::Word8
bK = 16::Word8
wP = 1::Word8
wR = 4::Word8
wN = 2::Word8
wB = 3::Word8
wQ = 5::Word8
wK = 6::Word8

showCoord :: Coord->String
showCoord (x,y) = ("abcdefgh" !! fromEnum x) : show (8-y)
showMove :: Move->String
showMove (piece, from, to) = (pieceToChar piece) : (showCoord from ++"-"++showCoord to)
showMoveInContext :: Board->Move->String
showMoveInContext board@Board{pieceArray=a} m@(piece, from, to) = case m of
   (6,(4,7),(6,7)) -> "O-O"
   (6,(4,7),(2,7)) -> "O-O-O"
   (16,(4,0),(6,0)) -> "O-O"
   (16,(4,0),(2,0)) -> "O-O-O"
   _ ->
      let
         p = a!from
         char = map toUpper $ if p==wP || p==bP then "" else (pieceToChar p):[]
         suffix = if p==piece then "" else '=':(pieceToChar piece):[]
      in
         char ++ (showCoord from ++ showCoord to ++ suffix)


pieceToChar :: Piece -> Char
pieceToChar c = " PNBRQK    pnbrqk" !! fromEnum c
charToPiece :: Char -> Piece
charToPiece c = (toEnum . fromJust)(elemIndex c " PNBRQK    pnbrqk")

emptyBoard = Board array False White True True True True Nothing (4,7) (4,0)
    where array = listArray ((0,0),(7,7)) [0..]

startingBoard = Board array False White True True True True Nothing (4,7) (4,0)
    where
        array = listArray ((0,0),(7,7)) ([
            14,11,0,0,0,0,1,4,
            12,11,0,0,0,0,1,2,
            13,11,0,0,0,0,1,3,
            15,11,0,0,0,0,1,5,
            16,11,0,0,0,0,1,6,
            13,11,0,0,0,0,1,3,
            12,11,0,0,0,0,1,2,
            14,11,0,0,0,0,1,4
            ] ::[Word8]) :: UArray(Word8,Word8)Word8
boardFromListOfStrings :: [String]->Board
boardFromListOfStrings ss = b{
      whiteKingAt = getUniquePiecePosition wK b,
      blackKingAt = getUniquePiecePosition bK b
    }
    where
      b = Board array False White True True True True Nothing (4,7) (4,0)
      array = listArray ((0,0),(7,7)) [
            charToPiece(ss!!y!!x)
            | x<-[0..7], y<-[0..7]
            ]


getPiece square (Board{pieceArray=a}) = a!square

putPiece square piece b@(Board{pieceArray=a}) =
    b{ pieceArray = a // [(square,piece)] }

doMove :: Move->Board->Board
doMove (p,from,to) b@(Board{pieceArray=a}) =
    b{
      whoseTurn = opponent $ whoseTurn b,
      pieceArray =
         if Just to == canDoEnpassantAt b && (p == 1 || p == 11)
         then a // [(from,0), (to,p), ((fst to, snd to + 1),0), ((fst to, snd to - 1),0)] --bicie w przelocie
         else if (p == 6 || p == 16)
            then if (fst to - fst from == -2)
               then a // [(from,0), (to,p), ((0,snd from),0), ((3,snd from),a!(0,snd from))] --roszada długa
               else if (fst to - fst from == 2)
                  then a // [(from,0), (to,p), ((7,snd from),0), ((5,snd from),a!(0,snd from))] --roszada krótka
               else a // [(from,0), (to,p)]
            else a // [(from,0), (to,p)]
      ,
      canWhiteCastleWest = canWhiteCastleWest b && from /= (4,7) && from /= (0,7),
      canWhiteCastleEast = canWhiteCastleEast b && from /= (4,7) && from /= (7,7),
      canBlackCastleWest = canBlackCastleWest b && from /= (4,0) && from /= (0,0),
      canBlackCastleEast = canBlackCastleEast b && from /= (4,0) && from /= (7,0),
      canDoEnpassantAt   = if (p == 1 || p == 11) && snd from - snd to == 2
         then Just $! (fst from, (snd from + snd to) `div` 2)
         else Nothing,
      whiteKingAt = if from == whiteKingAt b then to else whiteKingAt b,
      blackKingAt = if from == blackKingAt b then to else blackKingAt b

    }

isCoordLegal (x,y) = x>=0 && x<8 && y>=0 && y<8

pawnMoves :: Word8->[(Word8,Word8,Word8,Word8)] -> [Move]
pawnMoves _ [] = []
pawnMoves pawnOffset ((x, y, x', y'):xs)
    | y'==7||y'==0 = [(pawnOffset+t,(x,y),(x',y')) | t<-[2..5]]
        ++ (pawnMoves pawnOffset $! xs)
    | otherwise = (pawnOffset+1,(x,y),(x',y')):(pawnMoves pawnOffset xs)

getAllLegalMoves :: Board -> [Move]
getAllLegalMoves b = case whoseTurn b of
   White -> getAllLegalMovesForWhite b
   Black -> getAllLegalMovesForBlack b

getAllLegalMoves'ForWhite :: Board -> [Move]
getAllLegalMoves'ForWhite b@(Board{pieceArray=a}) =
    concat $! [ getLegalMoves' (x,y) b
    | x<-[0..7],
        y<-[0..7],
        isWhite(a!(x,y)) ]
getAllLegalMoves'ForBlack :: Board -> [Move]
getAllLegalMoves'ForBlack b@(Board{pieceArray=a}) =
    concat $! [ getLegalMoves' (x,y) b
    | x<-[0..7],
        y<-[0..7],
        isBlack(a!(x,y)) ]

getAllLegalMovesForBlack :: Board -> [Move]
getAllLegalMovesForBlack b@(Board{pieceArray=a}) =
    concat [ getLegalMoves (x,y) b
    | x<-[0..7],
        y<-[0..7],
        isBlack(a!(x,y)) ]

getAllLegalMovesForWhite :: Board -> [Move]
getAllLegalMovesForWhite b@(Board{pieceArray=a}) =
    concatMap (\f -> getLegalMoves f b)  [ (x,y)
    | x<-[0..7],
        y<-[0..7],
        isWhite(a!(x,y)) ]

--TODO: legalność roszad
getLegalMoves :: Coord->Board->[Move]
getLegalMoves f@(x,y) b@(Board{pieceArray=a}) =
    filter (\m->not $ isKingInCheck $ doMove m b) $! getLegalMoves' f b

-- TODO: roszada
getLegalMoves' :: Coord->Board->[Move]
getLegalMoves' f@(x,y) b@(Board{pieceArray=a}) =
    let
        af = (a!f)
        p = if af<10 then af else af-10
        pawnOffset = af-p
        pawnDirection = if af<10 then -1 else 1
        pawnStartingLine = if af<10 then 6 else 1
        killableF = if af<10 then isBlackKillable else isWhiteKillable
        canCastleWest = if af<10 then canWhiteCastleWest b else canBlackCastleWest b
        canCastleEast = if af<10 then canWhiteCastleEast b else canBlackCastleEast b
    in af `seq` case p of
        -- пешка
        1 -> moves ++ capturesLeft ++ capturesRight
            where
                leftCapture = (x-1,y+pawnDirection)
                rightCapture = (x+1,y+pawnDirection)
                moves =
                    if isEmpty (a!(x,y+pawnDirection))
                        then
                            if y==pawnStartingLine && isEmpty (a!(x,y+2*pawnDirection))
                                then pawnMoves pawnOffset $! [(x,y,x,y+pawnDirection),(x,y,x,y+2*pawnDirection)]
                                else pawnMoves pawnOffset $! [(x,y,x,y+pawnDirection)]
                        else []
                capturesLeft = if x>0 && (
                        killableF(a!leftCapture) ||
                        canDoEnpassantAt b == Just leftCapture
                        )
                    then pawnMoves pawnOffset $! [(x,y,x-1,y+pawnDirection)]
                    else []
                capturesRight = if x<7 && (
                        killableF(a!rightCapture) ||
                        canDoEnpassantAt b == Just rightCapture
                        )
                    then pawnMoves pawnOffset $! [(x,y,x+1,y+pawnDirection)]
                    else []
        -- конь
        2 -> map (\x->(af,f,x)) $!
            filter (\x -> killableF (a!x) || isEmpty (a!x)) $!
            filter isCoordLegal $! [(x+1,y-2),(x-1,y-2),(x+1,y+2),(x-1,y+2),
            (x+2,y-1),(x-2,y-1),(x+2,y+1),(x-2,y+1)]
        -- слон
        3 ->
            concatMap (\l ->
               getLegalMovesForRook killableF af f (l!f) b
            ) $! [scanListSW, scanListSE, scanListNW, scanListNE]
        -- ладья
        4 ->
            concatMap (\l ->
               getLegalMovesForRook killableF af f (l!f) b
            ) $! [scanListN, scanListE, scanListS, scanListW]
        -- ферзь
        5 ->
            concatMap (\l ->
               getLegalMovesForRook killableF af f (l!f) b
            ) $! [scanListN, scanListE, scanListS, scanListW, scanListSW, scanListSE, scanListNW, scanListNE]
        -- король
        6 -> castlings ++ normalMoves
            where
               normalMoves =
                  map  (\x->(af,f,x)) $!
                  filter (\x -> killableF (a!x) || isEmpty (a!x)) $!
                  filter isCoordLegal $!
                  [(x+1,y),(x,y+1),(x-1,y),(x,y-1),(x+1,y-1),(x+1,y+1),(x-1,y+1),(x-1,y-1)]
               castlings =
                  if canCastleWest || canCastleEast
                  then if not $ isKingLegallyInCheck b
                     then
                        let
                           longCastling = if canCastleWest
                              then if all isEmpty $ [a!(1,y),a!(2,y),a!(3,y)]
                                 then if isKingInCheck $ doMove (af,f,(3,y)) b
                                    then []
                                    else [(af,f,(2,y))]
                                 else []
                              else []
                           shortCastling = if canCastleWest
                              then if all isEmpty $ [a!(5,y),a!(6,y)]
                                 then if isKingInCheck $ doMove (af,f,(5,y)) b
                                    then []
                                    else [(af,f,(6,y))]
                                 else []
                              else []
                        in
                           longCastling ++ shortCastling
                     else []
                  else []
        _ -> []

getLegalMovesForRook killableF piece from [] b = []
getLegalMovesForRook killableF piece from (to:scanline) b@(Board{pieceArray=a}) =
   let
      target = (a!to)
      pft    = (piece, from, to)
   in
     target `seq`
     if isEmpty target
         then pft:(getLegalMovesForRook killableF piece from scanline b)
         else if killableF target
             then [pft]
             else []

getUniquePiecePosition :: Piece->Board->Coord
getUniquePiecePosition piece b@Board{pieceArray=a} =
    head [(x,y) | x<-[0..7], y<-[0..7], a!(x,y) == piece]

isKingInCheck :: Board->Bool
isKingInCheck b =
    isKingInCheck' checkWhiteKing ((if checkWhiteKing then whiteKingAt else blackKingAt) b) b
    where checkWhiteKing = whoseTurn b == Black
isKingLegallyInCheck :: Board->Bool
isKingLegallyInCheck b =
    isKingInCheck' checkWhiteKing ((if checkWhiteKing then whiteKingAt else blackKingAt) b) b
    where checkWhiteKing = whoseTurn b == White

isKingInCheck' :: Bool->Coord->Board->Bool
isKingInCheck' checkWhiteKing f@(x,y) b@Board{pieceArray=a} =
    let
        checkWhiteKing = whoseTurn b == Black
        l = if checkWhiteKing then bB else wB
        q = if checkWhiteKing then bQ else wQ
        r = if checkWhiteKing then bR else wR
        pawn'sY = if checkWhiteKing then y-1 else y+1
        threats = if checkWhiteKing then threatsForWhiteKing!f else threatsForBlackKing!f
    in
        anyThreat threats a ||
        scanFor r (scanListN ! f) b ||
        scanFor r (scanListS ! f) b ||
        scanFor r (scanListW ! f) b ||
        scanFor r (scanListE ! f) b ||
        scanFor q (scanListN ! f) b ||
        scanFor q (scanListS ! f) b ||
        scanFor q (scanListW ! f) b ||
        scanFor q (scanListE ! f) b ||
        scanFor q (scanListNW ! f) b ||
        scanFor q (scanListSW ! f) b ||
        scanFor q (scanListNE ! f) b ||
        scanFor q (scanListSE ! f) b ||
        scanFor l (scanListNW ! f) b ||
        scanFor l (scanListSW ! f) b ||
        scanFor l (scanListNE ! f) b ||
        scanFor l (scanListSE ! f) b

anyThreat [] a = False
anyThreat ((piece,f):ts) a = if (a!f) == piece then True else anyThreat ts a
threatsForWhiteKing :: Array (Word8,Word8) [(Piece, Coord)]
threatsForWhiteKing = array ((0,0),(7,7)) $! [((x,y),helper (x,y)) | x<-[0..7], y<-[0..7]]
   where
      helper (x,y) = filter (isCoordLegal . snd) $! [
         (bP, (x-1, y-1)), (bP, (x+1, y-1)),
         (bN, (x+1, y+2)), (bN, (x+2, y+1)),
         (bN, (x-1, y+2)), (bN, (x-2, y+1)),
         (bN, (x-1, y-2)), (bN, (x-2, y-1)),
         (bN, (x+1, y-2)), (bN, (x+2, y-1)),
         (bK, (x-1, y-1)), (bK, (x, y-1)), (bK, (x+1, y-1)),
         (bK, (x-1, y+0)),                 (bK, (x+1, y+0)),
         (bK, (x-1, y+1)), (bK, (x, y+1)), (bK, (x+1, y+1))
         ]
threatsForBlackKing :: Array (Word8,Word8) [(Piece, Coord)]
threatsForBlackKing = array ((0,0),(7,7)) $! [((x,y),helper (x,y))| x<-[0..7], y<-[0..7]]
   where
      helper (x,y) = filter (isCoordLegal . snd) $! [
         (wP, (x-1, y+1)), (wP, (x+1, y+1)),
         (wN, (x+1, y+2)), (wN, (x+2, y+1)),
         (wN, (x-1, y+2)), (wN, (x-2, y+1)),
         (wN, (x-1, y-2)), (wN, (x-2, y-1)),
         (wN, (x+1, y-2)), (wN, (x+2, y-1)),
         (wK, (x-1, y-1)), (wK, (x, y-1)), (wK, (x+1, y-1)),
         (wK, (x-1, y+0)),                 (wK, (x+1, y+0)),
         (wK, (x-1, y+1)), (wK, (x, y+1)), (wK, (x+1, y+1))
         ]

scanListN :: Array (Word8, Word8) [Coord]
scanListN = array ((0,0),(7,7)) $! [((x,y),tail $ helper (x,y))| x<-[0..7], y<-[0..7]]
   where
      helper (x,y) = if   isCoordLegal (x,y)
         then (x,y):(helper $! (x,y-1))
         else []
scanListNW :: Array (Word8, Word8) [Coord]
scanListNW = array ((0,0),(7,7)) $! [((x,y),tail $ helper (x,y))| x<-[0..7], y<-[0..7]]
   where
      helper (x,y) = if   isCoordLegal (x,y)
         then (x,y):(helper $! (x-1,y-1))
         else []
scanListS :: Array (Word8, Word8) [Coord]
scanListS = array ((0,0),(7,7)) $! [((x,y),tail $ helper (x,y))| x<-[0..7], y<-[0..7]]
   where
      helper (x,y) = if   isCoordLegal (x,y)
         then (x,y):(helper $! (x,y+1))
         else []
scanListSW :: Array (Word8, Word8) [Coord]
scanListSW = array ((0,0),(7,7)) $! [((x,y),tail $ helper (x,y))| x<-[0..7], y<-[0..7]]
   where
      helper (x,y) = if   isCoordLegal (x,y)
         then (x,y):(helper $! (x-1,y+1))
         else []
scanListSE :: Array (Word8, Word8) [Coord]
scanListSE = array ((0,0),(7,7)) $! [((x,y),tail $ helper (x,y))| x<-[0..7], y<-[0..7]]
   where
      helper (x,y) = if   isCoordLegal (x,y)
         then (x,y):(helper $! (x+1,y+1))
         else []
scanListNE :: Array (Word8, Word8) [Coord]
scanListNE = array ((0,0),(7,7)) $! [((x,y),tail $ helper (x,y))| x<-[0..7], y<-[0..7]]
   where
      helper (x,y) = if   isCoordLegal (x,y)
         then (x,y):(helper $! (x+1,y-1))
         else []
scanListE :: Array (Word8, Word8) [Coord]
scanListE = array ((0,0),(7,7)) $! [((x,y),tail $ helper (x,y))| x<-[0..7], y<-[0..7]]
   where
      helper (x,y) = if   isCoordLegal (x,y)
         then (x,y):(helper $! (x+1,y))
         else []
scanListW :: Array (Word8, Word8) [Coord]
scanListW = array ((0,0),(7,7)) $! [((x,y),tail $ helper (x,y))| x<-[0..7], y<-[0..7]]
   where
      helper (x,y) = if   isCoordLegal (x,y)
         then (x,y):(helper $! (x-1,y))
         else []

scanFor :: Piece->[Coord]->Board->Bool
scanFor _ [] _ = False
scanFor piece (f:fs) b@Board{pieceArray=a} =
   let
     p = a!f
   in p `seq` if p==0
      then scanFor piece fs b
      else if p==piece
          then True
          else False
{-scanFor :: Piece->Piece->Coord->(Word8,Word8)->Board->Bool
scanFor piece skip (x,y) (dx,dy) b@Board{pieceArray=a}=
    if isCoordLegal (x,y)
    then let
           p = a!(x,y)
         in p `seq` if p==0 || p==skip
            then scanFor piece 42 (x+dx,y+dy) (dx,dy) b
            else if p==piece
                then True
                else False
    else False-}
hasPiece :: Piece->Coord->Board->Bool
hasPiece piece f b@Board{pieceArray=a} =
   if isCoordLegal f
      then a!f == piece
      else False
