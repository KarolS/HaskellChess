module AltParser where

type ParseResult a = Either String a
data ParserStatus = ParserStatus {
   parserRest :: !String
   } deriving (Show)

data Parser a = Parser {applyParser::(ParserStatus -> (ParserStatus,ParseResult a))}

char :: Char -> Parser Char
char a = Parser (\status ->
   case parserRest status of
      [] -> (ParserStatus [], Left $ '`':a:"`")
      (x:tail) -> if x==a
         then (ParserStatus tail, Right $! a )
         else (ParserStatus (x:tail), Left $ '`':a:"`")
   )

anyChar :: Parser Char
anyChar = Parser (\status ->
      case parserRest status of
         [] -> (ParserStatus [], Left $! "any char")
         (x:tail) -> (ParserStatus tail, Right $! x)
   )

fromRight (Right r) = r

spaces :: Parser String
spaces = many $ char ' ' --TODO
many :: Parser a -> Parser [a]
many p = Parser (\status ->
   let (status',result) = p `applyParser` status
   in case result of
      Left _  -> (status,   Right $! [])
      Right x -> (status'', Right $! (x : fromRight results))
         where (status'',results) = (many p) `applyParser` status'
   )
many1 :: Parser a -> Parser [a]
many1 p = do
   x <- p
   xs <- many p
   return $! x:xs

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = Parser (\status ->
   let (status',result) = p `applyParser` status
   in case result of
      Left _  -> (status,  Right $! Nothing)
      Right x -> (status', Right $! Just x)
   )

eof :: Parser ()
eof = Parser (\s ->
   if parserRest s == "" then (s, Right $! ())
   else (s, Left "end of file") )
string :: String -> Parser String
string [] = return ""
string (x:xs) = liftError ('`':x:xs ++ "'") $ do
   y <- char x
   ys <- string xs
   return $! y:ys

oneOf :: String -> Parser Char
oneOf []     = error "Calling oneOf with empty argument"
oneOf (x:[]) = char x
oneOf (x:xs) = liftError ("any of "++('`':x:xs)++"'") $ char x <|> oneOf xs

noneOf :: String -> Parser Char
noneOf forbiddenChars = Parser (\status ->
   case parserRest status of
      [] -> (ParserStatus [], Left $ "any char except `"++forbiddenChars++"'")
      (x:tail) -> if x `elem` forbiddenChars
         then (ParserStatus (x:tail), Left $ "any char except `"++forbiddenChars++"'")
         else (ParserStatus tail, Right $! x )
   )

digit = oneOf "1234567890"

liftError :: String -> Parser a -> Parser a
liftError newErr p = Parser (\s ->
      let (s',r) = p `applyParser` s
      in case r of
         Left err -> (s', Left newErr)
         Right  _ -> (s', r)
   )

instance Monad Parser where
   return x = Parser (\s -> (s,Right x))
   p1 >>= p2 = Parser(\s ->
      let (s', result) = p1 `applyParser` s
      in case result of
         Left err -> (s, Left err)
         Right x ->
            let (s'',result2) = (p2 x) `applyParser` s'
            in case result2 of
               Left err -> (s, Left err)
               Right x -> (s'',Right $! x)
      )

instance Functor Parser where
   fmap f p = do
      x <- p
      return $! f x

p <|> q = Parser(\s ->
   let (s',result) = p `applyParser` s
   in case result of
      Left err ->
         let (s'',result') = q `applyParser` s
         in case result' of
            Left err' -> (s, Left (err++" or "++err'))
            Right _ -> (s'', result')
      Right _ -> (s', result) )

try = id

data ParseError =
   ParseError String Int String
instance Show ParseError where
   show (ParseError sourceName i err) = sourceName ++ ":" ++ (show i) ++": expected "++err
parse :: Parser a -> String -> String -> Either ParseError a
parse parser sourceName text =
   case (applyParser parser) (ParserStatus text) of
      (rest, Left err) -> Left $ ParseError sourceName (length text - length (parserRest rest)) err
      (_, Right reslt) -> Right $! reslt

