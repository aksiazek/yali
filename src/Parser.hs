module Parser where

import Expressions
import Control.Monad.Error
import Text.Parsec

-- Parsing s-expressions

parse :: String -> LispResult
parse source = case (Text.Parsec.parse parseAll "" source) of
		 Right x -> return x
		 Left err -> throwError $ show err

parseAll = do spaces
              x <- parseExpr
	      spaces
	      return x

parseExpr = parseExprNoBlanks <|> parseBlank
            
parseExprNoBlanks = try parseInteger
                    <|> try parseSymbol
                    <|> try parseList
                                
parseBlank = do comment <- option "" (string ";")
                when (comment == ";") $ do
                  skipMany anyToken
                notFollowedBy anyToken
                return Blank

parseInteger = do sign <- option "" (string "-")
		  number <- many1 digit
		  return $ LispInt (read (sign++number))

parseSymbol = do f <- firstAllowed
		 r <- many $ firstAllowed <|> digit
		 return $ LispSymbol (f:r)
	where firstAllowed = oneOf "+-*/!@#$%^&=[]{}:<>,.?\\|~`'\"" <|> letter

parseList = do char '('
               spaces
	       x <- parseExprNoBlanks `sepEndBy` spaces   
               char ')'
	       return $ LispList x



