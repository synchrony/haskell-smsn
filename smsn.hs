import Control.Monad (void)
import Control.Applicative (empty)
import Data.Void (Void)
import Data.Maybe (Maybe(..),maybeToList)

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

word :: String -> Parser String
word = L.symbol sc

anyWord :: Parser String
anyWord = lexeme $ some C.alphaNumChar

restOfLine :: Parser String
restOfLine = many $ C.satisfy $ not . (== '\n')

idString :: Parser String
idString = count 16 C.alphaNumChar


type PropertyName = String

data HalfParsed = HalfText String
                | HalfChildren [String]
                | OneLiner PropertyName String deriving Show

oneLiner :: Parser HalfParsed
oneLiner =  OneLiner <$> (C.char '@' *> anyWord)
                     <*> (restOfLine <* C.newline)

tOneLiner = parseTest (many oneLiner)
  "@what thingo\n@ner dingo\n@zup narbo narb narb\n"

halfText :: Parser HalfParsed
halfText = HalfText <$> do
  ( C.string "@text ```\n"
    *> C.anyChar `manyTill` C.string "\n```\n" )

tHalfText = parseTest halfText "@text ```\nlalala \n lalalala \n```\n"

halfChildren :: Parser HalfParsed
halfChildren = HalfChildren <$>
  many (C.string "* :" *> idString <* C.string ": \n")

tHalfChildren = parseTest halfChildren "* :asdfyuioasdf7890:\n* :asdfyuioqwer1234:"

data Property = SmsnId String
              | Title String
              | Created Int
              | Weight Float
              | Alias String
              | Other PropertyName String
              | SmsnText String
              | Children [String] deriving Show

smsnProperty :: HalfParsed -> Property
smsnProperty s = case s of
  OneLiner n t -> case n of
    "id" -> SmsnId t
    "title" -> Title t
    "created" -> Created (read t :: Int)
    "weight" -> Weight (read t :: Float)
    "alias" -> Alias t
    _ -> Other n t
  HalfText t -> SmsnText t
  HalfChildren ts -> Children ts

note :: Parser [Property]
note = do
  oneLiners <- many oneLiner
  text <- either (const Nothing) Just <$> observing (try halfText)
  children <- halfChildren
  atEnd
  return $ map smsnProperty $
    concat [oneLiners
           , maybeToList text
           , [children]
           ]

-- whole-file tests
test :: String -> IO ()
test s = do
  file <- readFile s
  parseTest note file

fileWithText =  "/home/jeff/smsn+/kb/vcs/public/kh667uKCjKn8WzfB.smsn"
fileWithNoText = "/home/jeff/smsn+/kb/vcs/public/zzYSwIonHKgH4EYm.smsn"

-- test fileWithNoText
-- test fileWithText
