#include "common.h"

module Main (main) where
import Chess
import Ai
import IterDeep
import Data.Word
import Data.Array.Base
import Data.Maybe
import Control.Monad hiding (forM_)
import System.IO
import UCI
import Utils
import Control.Concurrent
import Data.IORef
import Uci2Chess
import Data.List
import Data.Foldable hiding (foldl')

data InnerState = InnerState {
   runningState :: Bool,
   aiThread :: Maybe ThreadId,
   aiResult :: IORef (Maybe GameTree),
   aiPlayer :: Player,
   aiBoard :: Board,
   logFile :: Handle
}

bPutStrLn handle string = do
   hPutStrLn handle string
   hFlush handle
   putStrLn string
   hFlush stdout

aiThreadProcedure innerstate = do
   writeIORef (aiResult innerstate) (Just $ createInitialTree $ aiBoard innerstate)
   aiThreadProcedure' innerstate{aiPlayer = whoseTurn $ aiBoard innerstate}

aiThreadProcedure' innerstate = do
   modifyIORef (aiResult innerstate) (Just . cull . deepen 0.0 . fromJust)
   aiThreadProcedure' innerstate

joinAiThread :: ThreadId -> InnerState -> IO (Maybe Move)
joinAiThread thId innerstate = do
   let ioTreeRef = aiResult innerstate
   let isWhite = aiPlayer innerstate
   maybetree <- readIORef ioTreeRef
   case maybetree of
      Nothing -> joinAiThread thId innerstate
      Just tree -> do
         killThread thId
         maybetree <- readIORef ioTreeRef
         return $ pickBestMoveFromTree isWhite $ fromJust maybetree

killAiIfNeeded :: InnerState -> IO InnerState
killAiIfNeeded innerstate = case aiThread innerstate of
   Nothing -> return innerstate
   Just th -> killThread th >> return innerstate{aiThread=Nothing}

initialInnerState = do
   ioref <- newIORef Nothing
   log <- openFile "/tmp/chess.log" WriteMode
   return $ InnerState True Nothing ioref Black startingBoard log

reactToUci :: UciQuery -> InnerState -> IO InnerState
reactToUci UciHello s = do
   bPutStrLn (logFile s) "id name Haskak 0.1"
   bPutStrLn (logFile s) "id author Karol M. Stasiak"
   bPutStrLn (logFile s) "uciok"
   return s
reactToUci UciInvalid s = return s
reactToUci (UciDebug _) s = bPutStrLn (logFile s) "uciok" >> return s
reactToUci UciIsready s = bPutStrLn (logFile s) "readyok" >> return s
reactToUci (UciSetoption _ _) s = return s
reactToUci UciQuit s = killAiIfNeeded s{runningState=False}
reactToUci UciNewgame s = bPutStrLn (logFile s) "readyok" >> killAiIfNeeded s{aiBoard = startingBoard}
reactToUci (UciPosition p moves) s =
   let board = runUciPosition p moves
   in
      killAiIfNeeded s{aiBoard = board}

reactToUci (UciGo opts) s =
   if opts == "infinite" || opts == " infinite"
      then do
         thId <- forkIO $ aiThreadProcedure s
         return $ s {aiThread = Just thId}
      else do
         case  pickMove 2 $ aiBoard s of --TODO
            Just move -> do
               replyWithMove move (aiBoard s) (logFile s)
               let newBoard = doMove move $ aiBoard s
               return s {aiBoard = newBoard}
            Nothing -> return s

reactToUci UciStop s  = do
   s' <- killAiIfNeeded s
   maybeTree <- readIORef $ aiResult s'
   forM_ (maybeTree >>= pickBestMoveFromTree (aiPlayer s')) (\m -> replyWithMove m (aiBoard s') (logFile s'))
   return s'
reactToUci a s = do
   bPutStrLn (logFile s) $ show a
   error $ show a

replyWithMove :: Move -> Board -> Handle-> IO ()
replyWithMove move oldBoard h = do
   bPutStrLn h $ "bestmove " ++ moveToUci move oldBoard

loop :: InnerState -> IO()
loop state = if runningState state
   then do
      line <- getLine
      hPutStrLn (logFile state) $ '>':line
      let uci = parseUciQuery line
      --hPutStrLn (logFile state) $ '>':(show uci)
      hFlush $ logFile state
      state' <- reactToUci uci state
      loop state'
   else return ()

main = do
   s <- initialInnerState
   loop s
