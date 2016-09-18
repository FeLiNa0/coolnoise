module Cool.Lex (coolLexer, printLexResult) where

import Data.Char
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L

import Cool.Types (Expr)


lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

stringNoCase = string'

reservedWords = map (map toLower)
  -- NOTE!!!!!!!!!! lexer had to parse inherits before in
  ["class", "else", "fi", "if", "inherits",
   "in", "isvoid", "let", "loop", "pool", "then",
   "while", "case", "esac", "new", "of", "not"]

reservedIdentifiers = map (map toLower) ["true", "false"]

reservedSymbols = ["<-", "@", ".", "{", "}", "(", ")", ";", ",", ":",
                   "+", "=>", "-", "*", "/", "~", "<", "<=", "="]

-- The lexical units of Cool are integers, type identifiers, object
-- identifiers, special notation, strings, keywords, and white space.
spaceConsumer :: Parser ()
spaceConsumer =
  -- "White space consists of any sequence of the characters: blank
  -- (ascii 32), \n (newline, ascii 10), \f (form feed, ascii 12), \r
  -- (carriage return, ascii 13), \t (tab, ascii 9), \v (vertical tab,
  -- ascii 11)."
  L.space (void spaceChar)

  -- "Any characters between two dashes “--” and the next newline (or
  -- EOF, if there is no next newline) are treated as comments."
  (L.skipLineComment "--")

  -- "Comments may also be written by enclosing
  -- text in (∗ ... ∗). [This] form of comment may be nested."
  (L.skipBlockCommentNested "(*" "*)")

-- "Integers are non-empty strings of digits 0-9."
parseInteger = lexeme L.integer

parseString = between (symbol "\"") (symbol "\"")
  -- no unicode, no unprintable characters, no ", no \
            $ many (oneOf (map Data.Char.chr $ [32..33] ++ [35..91] ++ [93..126])
                  <|> (escapeChar <$> (char '\\' *> anyChar)))
  -- TODO backslash
  where escapeChar 'n' = '\n'
        escapeChar c = c

identChar :: Parser Char
identChar = satisfy (\c -> isAlphaNum c || '_' == c)
         <?> "alphanumeric character or underscore"

-- "Identifiers are strings (other than keywords) consisting of
-- letters, digits, and the underscore character."
-- "object identifiers begin with a lower case letter."

-- TODO QUESTION: can identifiers (not type or object ids) start with
-- an identChar that isn't a lowercase letter?
identifier = lexeme (parseIdent >>= isNotReserved)
  where parseIdent = (:) <$> lowerChar <*> many identChar
        isNotReserved str =
          if map toLower str `elem` (reservedIdentifiers ++ reservedWords)
          then fail $ "keyword " ++ show str ++ " is reserved"
          else return str

-- "Type identifiers begin with a capital letter..."
-- NOTE: type identifiers can be reserved words
-- example:  `class Where {};`
typeIdentifier = lexeme ((:) <$> upperChar <*> many identChar)

-- "Except for the constants true and false, keywords are case insensitive."
reservedWord w = stringNoCase w *>
  notFollowedBy identChar *> spaceConsumer *> pure w

-- "To conform to the rules for other objects, the first letter of true
-- and false must be lowercase; the trailing letters may be upper or
-- lower case."
reservedIdentifier (c:w) = satisfy (== (toLower c)) *> stringNoCase w *>
  notFollowedBy identChar *> spaceConsumer *> pure (c:w)

data CoolToken = Symbol String
               | ReservedWord String
               | Identifier String
               | TypeIdentifier String
               | Int Integer
               | String String
               | Error String
               deriving (Eq, Show)

coolLexer :: Parser [(SourcePos, CoolToken)]
coolLexer = spaceConsumer *> (many $ do
  p <- getPosition
  choice $
    -- TODO *) should give error
    -- TODO how do string and string' backtrack efficiently?
       [pure (tag p Error $ "unmatched *)") <* symbol "*)"]
    ++ (map ((tag p ReservedWord <$>) . try . reservedWord)       reservedWords)
    ++ (map ((tag p ReservedWord <$>) . try . reservedIdentifier) reservedIdentifiers)
    ++ (map ((tag p Symbol       <$>) . try . symbol)             reservedSymbols)
    ++ [ try $ tag p Int <$> parseInteger
       , try $ tag p TypeIdentifier <$> typeIdentifier
       , try $ tag p String <$> parseString
       , try $ tag p Identifier <$> identifier
       ]
  ) <* eof
  where tag position t s = (position, t s)


printLexResult :: Either String [(SourcePos, CoolToken)] -> IO ()
printLexResult = either putStrLn (mapM_ (
  putStrLn . (\(pos, t) -> sourcePosPretty pos ++ ": " ++ show t)))
