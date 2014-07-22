module Std where

import Expressions
import Eval
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error

lispArithmetic f  = do (LispList args) <- getSymbol "..."
                       lispBinary f args

lispBinary :: (Integer->Integer->Integer) -> [LispExpr] -> LispResult
lispBinary op args = do return $ foldl1 (lispBinaryAux op) args
	where lispBinaryAux op (LispInt i) (LispInt j) = LispInt (i `op` j)

-- Equality
lispEq = do (LispList args) <- getSymbol "..."  
            action args
            where action args
                      | length args /= 2 = throwError "Invalid number of arguments"
                      | otherwise = return $ foldl1 isEqual args

isEqual :: LispExpr -> LispExpr -> LispExpr
isEqual (LispSymbol a) (LispSymbol b) = LispSymbol(if a == b then "t" else "nil")
isEqual (LispInt a) (LispInt b) = LispSymbol(if a == b then "t" else "nil")
isEqual (LispList a) (LispList b) = LispSymbol(if (null a) && (null b) then "t" else "nil")
isEqual _ _ = LispSymbol "nil"

-- Quoting
lispQuote = do (LispList arg) <- getSymbol "..."  
               action arg
                   where action arg
                             | length arg /= 1 = throwError "Invalid number of arguments"
                             | otherwise = return $ head arg
        
lispAtom = do (LispList arg) <- getSymbol "..."  
              action arg
                  where action arg
                            | length arg /= 1 = throwError "Invalid number of arguments"
                            | isAtom $ head arg = return $ LispSymbol "t"
                            | otherwise = return $ LispSymbol "nil"

isAtom :: LispExpr -> Bool
isAtom (LispList list) = null list
isAtom _ = True

isList :: LispExpr -> Bool
isList (LispList list) = True
isList _ = False

lispCar = do (LispList list) <- getSymbol "..."  
             action list
                 where action arg
                           | length arg /= 1 = throwError "Invalid number of arguments"
                           | head arg == (LispList []) = return $ (LispSymbol "nil")
                           | isAtom $ head arg = throwError "Argument is not of type list" 
                           | otherwise = return $ car $ head arg

lispCdr = do (LispList list) <- getSymbol "..."  
             action list
                 where action arg
                           | length arg /= 1 = throwError "Invalid number of arguments"
                           | head arg == (LispList []) = return $ (LispSymbol "nil")
                           | isAtom $ head arg = throwError "Argument is not of type list" 
                           | otherwise = return $ cdr $ head arg

car :: LispExpr -> LispExpr
car (LispList list) = head list
car _ = (LispSymbol "nil")

cdr :: LispExpr -> LispExpr
cdr (LispList list) = (LispList (tail list))
cdr _ = (LispSymbol "nil")

lispCons = do (LispList args) <- getSymbol "..."  
              action args
                  where action args
                            | length args /= 2 = throwError "Invalid number of arguments"
                            | not $ isList $ head $ tail args = throwError "Second arg required to be a list"
                            | otherwise = return $ splice (head args) (head $ tail args)  

splice :: LispExpr -> LispExpr -> LispExpr
splice elem (LispList list) = (LispList (elem:list))
splice _ _ = (LispSymbol "nil")

-- Set modifies context, adds variable
lispSetArgs = ["symbol", "value"]
lispSet = do [(LispSymbol s), e] <- getSymbols lispSetArgs
             eval_e <- eval e
             updateSymbolInParent s eval_e
             return eval_e

-- If Conditional
lispIfArgs = ["condition", "expr1", "expr2"]
lispIf = do [condExpr, expr1, expr2] <- getSymbols lispIfArgs
            eval_cond <- eval condExpr
            if (notNil eval_cond) then eval expr1
                                        else eval expr2
    where notNil (LispSymbol val) = "nil" /= val

-- Lambda function creation
lispLambdaArgs = ["args", "..."]
lispLambda = do [(LispList args), (LispList body)] <- getSymbols lispLambdaArgs
                let newFn = do evalBody <- mapM eval body
                               return $ last evalBody
                return $ LispLambda newFn (map show args)

-- Function creation
lispFunArgs = ["name", "args", "..."]
lispFun = do [(LispSymbol name), (LispList args), (LispList body)] <- getSymbols lispFunArgs
             let newFn = do evalBody <- mapM eval body
                            return $ last evalBody
             let lambda = LispFunc newFn name (map show args)
             updateSymbolInParent name lambda
             return lambda

-- Symbol table
initialCtx = Ctx (Map.fromList 
                         [
                          ("t", LispSymbol "t"),
                          ("nil", LispSymbol "nil"),
			  ("quote", LispSpecial lispQuote ["..."]),
			  ("atom", LispFunc lispAtom "atom" ["..."]), 
			  ("eq", LispFunc lispEq "eq" ["..."]),
			  ("car", LispFunc lispCar "car" ["..."]),
			  ("cdr", LispFunc lispCdr "cdr" ["..."]),
                          ("cons", LispFunc lispCons "cons" ["..."]),
			  ("+", LispFunc (lispArithmetic (+)) "+" ["..."]),
			  ("-", LispFunc (lispArithmetic (-)) "-" ["..."]),
			  ("*", LispFunc (lispArithmetic (*)) "*" ["..."]),
			  ("/", LispFunc (lispArithmetic div) "/" ["..."]),
			  ("setq", LispSpecial lispSet lispSetArgs),
                          ("if", LispSpecial lispIf lispIfArgs),
                          ("lambda", LispSpecial lispLambda lispLambdaArgs ),
                          ("defun", LispSpecial lispFun lispFunArgs )
			 ]) Nothing

-- Helper
getSymbol sym = eval $ (LispSymbol sym)
getSymbols syms = mapM getSymbol syms
