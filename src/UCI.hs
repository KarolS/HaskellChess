module UCI where

--import Text.ParserCombinators.Parsec
import AltParser
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Either
import Data.Char

data UciPos =
   UciStartpos |
   UciFen String String String String String String
   deriving Show

data UciMove =
   UciNormalMove Char Int Char Int |
   UciPromotionMove Char Int Char Int Char |
   UciNullmove
   deriving Show

type UciGoOptions = String

data UciQuery =
   UciHello |
   UciDebug Bool |
   UciIsready |
   UciSetoption String String |
   UciRegister String |
   UciNewgame |
   UciPosition UciPos [UciMove] |
   UciGo UciGoOptions |
   UciStop |
   UciPonderhit |
   UciQuit |
   UciInvalid
   deriving Show

uciBool :: Parser Bool
uciBool = do
   b <- try (string "true") <|> try (string "false") <|> try (string "false") <|> try (string "false")
   return $ b == "true" || b == "on"

uciPos :: Parser UciPos
uciPos = (string "startpos" >> return UciStartpos) <|> do
   s1 <- many $ noneOf " "
   spaces
   s2 <- many $ noneOf " "
   spaces
   s3 <- many $ noneOf " "
   spaces
   s4 <- many $ noneOf " "
   spaces
   s5 <- many $ noneOf " "
   spaces
   s6 <- many $ noneOf " "
   return $ UciFen s1 s2 s3 s4 s5 s6

uciMove :: Parser UciMove
uciMove =
   (string "0000" >> return UciNullmove) <|>
   ( do
      x1 <- oneOf"abcdefgh"
      y1 <- fmap digitToInt digit
      x2 <- oneOf"abcdefgh"
      y2 <- fmap digitToInt digit
      z <- optionMaybe(oneOf "qrnb")
      case z of
         Nothing -> return $ UciNormalMove x1 y1 x2 y2
         Just p  -> return $ UciPromotionMove x1 y1 x2 y2 p
   )



uciHello = string "uci" >> return UciHello
uciDebug = string "debug" >> spaces >> uciBool >>= \x -> return $ UciDebug x
uciIsready = string "isready" >> return UciIsready
uciSetoption = do
   string "setoption" >> spaces >> string "name" >> spaces
   many anyChar
   return $ UciSetoption "" ""
   --TODO
uciRegister = string "register" >> spaces >> many anyChar >>= \x -> return $ UciRegister x
uciNewgame = string "ucinewgame" >> return UciNewgame
uciMoves = many (spaces >> uciMove)
uciPosition = do
   string "position" >> spaces
   pos <- uciPos
   optionMaybe $ try (spaces >> string "moves")
   moves <- uciMoves
   return $ UciPosition pos moves --TODO

uciGo = string "go" >> many anyChar >>= \x -> return $ UciGo x --TODO
uciStop = string "stop" >> return UciStop
uciPonderhit = string "ponderhit" >> return UciPonderhit
uciQuit = string "quit" >> return UciQuit
uciInvalid = many anyChar >> return UciInvalid



uciQuery = try uciNewgame <|> try uciHello <|> try uciDebug <|> try uciIsready <|>
   try uciSetoption <|> try uciRegister <|>
   try uciPosition <|> try uciGo <|> try uciStop <|> try uciPonderhit <|>
   try uciQuit <|> try uciInvalid

parseUciQuery :: String -> UciQuery
parseUciQuery query = case parse uciQuery "" query of
   Left err -> error $ show err
   Right pq -> pq
parseUciMoves :: String -> [UciMove]
parseUciMoves string = case parse uciMoves "" string of
   Left err -> error $ show err
   Right pq -> pq
