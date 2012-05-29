#include "common.h"

module IterDeep (
   pickMove,
   GameTree,
   createInitialTree,
   deepen,
   pickBestMoveFromTree,
   cull) where
import Chess
import Utils
import Evaluators
import Data.Maybe
import Debug.Trace
import Debute
data Item = Item {
   startingMove :: Move,
   interesting :: Double,
   value :: Double,
   board :: Board
} deriving (Show)

data GameTree = GameLeaf Item | GameNode Player Double [GameTree] deriving (Show)

treeSize (GameLeaf _) = 1
treeSize (GameNode _ _ xs) = sum $ map treeSize xs

constructGameNode p = \ts -> GameNode p (treeValue' p ts) ts
constructItem startingMove move oldBoard =
   let
      newBoard = doMove move $! oldBoard
      intNVal = evaluateBoard newBoard
   in
      Item startingMove (fst $! intNVal) (snd intNVal) newBoard

pickMove :: Int -> Board -> Maybe Move
pickMove depth board =
   let
      tree = createInitialTree $! board
      who  = whoseTurn $! board
   in
      case tree of
         GameNode _ _ []  -> Nothing
         GameNode _ _ [x] -> pickAnyMoveFromTree x
         _ ->
               tree |>
               ((cull . deepen 0.0) ^/^ depth) |>
               --(deepen 2.0) |>
               (deepenToPlayer who) |>
               pickBestFromTree who |> fmap startingMove

createInitialTree board =
   board |> getAllLegalMoves |>
   map (\m ->
      GameLeaf $ constructItem m m board
   ) |> constructGameNode (whoseTurn board)

pickAnyMoveFromTree :: GameTree -> Maybe Move
pickAnyMoveFromTree (GameLeaf item) =
   Just $ startingMove item
pickAnyMoveFromTree (GameNode _ _ list) = pickAnyMoveFromTree' list

pickAnyMoveFromTree' [] = Nothing
pickAnyMoveFromTree' (x:xs) = case pickAnyMoveFromTree x of
   Nothing -> pickAnyMoveFromTree' xs
   just    -> just

pickBestMoveFromTree :: Player -> GameTree -> Maybe Move
pickBestMoveFromTree player (GameLeaf item) =
   Just $ startingMove item
pickBestMoveFromTree player tree@(GameNode _ _ list) = case list of
   [] -> Nothing
   subtree:[] -> pickAnyMoveFromTree subtree --mamy tylko jeden możliwy ruch
   _ ->
      tree |>
      pickBestFromTree player |> fmap startingMove

pickBestFromTree :: Player -> GameTree -> Maybe Item
pickBestFromTree player (GameLeaf item)  = Just item
pickBestFromTree player (GameNode _ _ trees) =
   trees |> map (pickBestFromTree $! opponent player) |>
   catMaybes |> maybeMaxBy (negIf (player==Black) . value)



flatTree :: GameTree -> [Item]
flatTree (GameLeaf i)  = [i]
flatTree (GameNode _ _ is) = concatMap flatTree $! is

deepenToPlayer :: Player -> GameTree -> GameTree
deepenToPlayer player l@(GameLeaf item) =
   if player /= whoseTurn (board item)
      then deepen' (-100000000.0) l
      else l
deepenToPlayer player (GameNode p _ xs) =
   constructGameNode p $ map (deepenToPlayer player) $! xs


deepen :: Double -> GameTree -> GameTree
deepen bar tree =
   let
      avgInt = flatTree tree |> map interesting |> avg
   in
      trace ("info string tree size: "++show (treeSize tree)) $
      deepen' (avgInt+bar) tree

deepen' :: Double -> GameTree -> GameTree
deepen' avgInt (GameNode p _ xs) =
   xs |> map (deepen' avgInt) |>
   constructGameNode p
deepen' avgInt (GameLeaf item) =
         if interesting item >= avgInt
         then
            let allMoves = getAllLegalMoves $! board item
            in case allMoves of
               [] -> GameLeaf item
               _  -> allMoves |> map (\move ->
                        constructItem (startingMove item) move (board item)
                     ) |> map GameLeaf |>
                     constructGameNode (whoseTurn $ board item)
         else GameLeaf item

treeValue' p    []  = 0.0
treeValue' p (x:[]) = treeValue x
treeValue' p (x:xs) =
   foldl (if p==White then max else min) (treeValue $! x) $!
   map treeValue $! xs

treeValue (GameLeaf item)     = value item
treeValue (GameNode _ v _)    = v

biasedTreeValue p = negIf (p==Black) . treeValue

cull :: GameTree -> GameTree
cull l@(GameLeaf _) = l
cull n@(GameNode _ _ []) = n
cull (GameNode p _ xs) =
   let
      culledXs = map cull $! xs
      lxs = fromIntegral $ length xs
      avgVal =
         (avg $ map (biasedTreeValue p) $! xs) +
         (-1.0) - 6*(log(1.0+1.0/lxs)/log 2)
   in
      constructGameNode p $! filter (
            (>=avgVal) . (negIf (p==Black)) . (biasedTreeValue p)
         ) $! xs


