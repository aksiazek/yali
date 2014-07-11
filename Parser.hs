module Parser where

import Expressions
import Control.Monad.Error
import Text.Parsec

-- Parsing s-expressions

parse :: String -> LispResult
parse source = case (Text.Parsec.parse parseExpr "" source) of
		 Right x -> return x
		 Left e -> throwError $ show e

parseExpr = do skipMany space
	       x <- parseExprPossible
	       skipMany space ; eof
	       return x

parseExprPossible = (try parseInteger) <|> (try parseSymbol) <|> (try parseList)  <|> (try parseBlank)

parseInteger = do sign <- option "" (string "-")
		  number <- many1 digit
		  return $ LispInt (read (sign++number))

parseSymbol = do f <- firstAllowed
		 r <- many (firstAllowed <|> digit)
		 return $ LispSymbol (f:r)
	where firstAllowed = oneOf "+-*/" <|> letter

parseBlank = do skipMany space
                return Blank

parseList = do char '(' ; skipMany space
	       x <- parseExprPossible `sepEndBy` (many1 space)
	       char ')'
	       return $ LispList x




