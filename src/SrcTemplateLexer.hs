
module SrcTemplateLexer
(
   stringLit,
   intLit,
   identifier,
   symbol,
   reserved,
   whiteSpace,
   reservedOp,
   parens,
   squares
)
where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(javaStyle)


srcTemplateLexer = P.makeTokenParser srcTemplateDef

srcTemplateDef = javaStyle
   {
      P.reservedNames = ["params", "$$", "true", "false", "if", "then", "else", "def", "options",
                         "description", "history"],
      P.reservedOpNames = ["+", "-", "*", "/", "%", "and", "or", "not"],
      P.caseSensitive  = True,
      P.opLetter = oneOf $ concat $ P.reservedOpNames srcTemplateDef,
      P.commentStart = "",
      P.commentEnd = "",
      P.commentLine = ""
   } 
   
intLit = P.integer srcTemplateLexer
stringLit = P.stringLiteral srcTemplateLexer
identifier = P.identifier srcTemplateLexer
symbol = P.symbol srcTemplateLexer
reserved = P.reserved srcTemplateLexer   
whiteSpace = P.whiteSpace srcTemplateLexer
reservedOp = P.reservedOp srcTemplateLexer
parens = P.parens srcTemplateLexer
squares = P.squares srcTemplateLexer

