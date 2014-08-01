module Std where

import Expressions
import Eval
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error

true = "t"
false = "nil"
nothing = "nil"

-- Helper functions
getSymbol sym = eval $ LispSymbol sym
getSymbols syms = mapM getSymbol syms

-- Primitives
lispQuote = getSymbol "arg" >>= return

lispAtom = do (LispList arg) <- getSymbol "..."  
              action arg
                  where action arg
                            | length arg /= 1 = throwError "Invalid number of arguments"
                            | isAtom $ head arg = return $ LispSymbol true
                            | otherwise = return $ LispSymbol false

isAtom :: LispExpr -> Bool
isAtom (LispList _) = False
isAtom _ = True

lispEq = do (LispList args) <- getSymbol "..."  
            action args
            where action args
                      | length args /= 2 = throwError "Invalid number of arguments"
                      | otherwise = return $ foldl1 isEqual args

isEqual :: LispExpr -> LispExpr -> LispExpr
isEqual (LispSymbol a) (LispSymbol b) = LispSymbol(if a == b then true else false)
isEqual (LispInt a) (LispInt b) = LispSymbol(if a == b then true else false)
isEqual (LispList a) (LispList b) = LispSymbol(if (null a) && (null b) then true else false)
isEqual _ _ = LispSymbol false

lispCar = do (LispList list) <- getSymbol "..."  
             action list
                 where action arg
                           | length arg /= 1 = throwError "Invalid number of arguments"
                           | head arg == (LispList []) = return $ (LispSymbol nothing)
                           | isAtom $ head arg = throwError "Argument is not of type list" 
                           | otherwise = return $ car $ head arg

lispCdr = do (LispList list) <- getSymbol "..."  
             action list
                 where action arg
                           | length arg /= 1 = throwError "Invalid number of arguments"
                           | head arg == (LispList []) = return $ head arg
                           | isAtom $ head arg = throwError "Argument is not of type list" 
                           | otherwise = return $ cdr $ head arg

car :: LispExpr -> LispExpr
car (LispList list) = head list
car _ = (LispSymbol nothing)

cdr :: LispExpr -> LispExpr
cdr (LispList list) = (LispList (tail list))
cdr _ = (LispSymbol nothing)

notNothing :: LispExpr -> Bool
notNothing = (/= (LispSymbol nothing))
            
lispCons = do (LispList args) <- getSymbol "..."  
              action args
                  where action args
                            | length args /= 2 = throwError "Invalid number of arguments"
                            | (isAtom $ last args) && (notNothing $ last args)
                                = throwError "Cannot construct list"
                            | otherwise = return $ splice (head args) (last args)  

splice :: LispExpr -> LispExpr -> LispExpr
splice elem (LispSymbol nothing) = (LispList [elem])
splice elem (LispList list) = (LispList (elem:list))
splice _ _ = (LispSymbol false)

lispCond = do (LispList args) <- getSymbol "..."  
              cond args
              where cond args
                         | null args = return $ (LispSymbol nothing)
                         | isAtom $ head args = throwError "cond clause is not a list"        
                         | notTwoElementList $ head args = throwError "clause has not two arguments"
                         | otherwise = do 
                      eval_expr <- eval $ car $ head args
                      if (not $ isBool $ eval_expr) then throwError $ "first form in a clause does not evaluate to " ++ true ++ " or " ++ false 
                      else if (eval_expr == (LispSymbol true)) 
                           then eval $ car $ cdr $ head args
                           else cond $ tail args

isBool :: LispExpr -> Bool
isBool (LispSymbol sym) = if(sym == true || sym == false) then True
                          else False
isBool _ = False 

notTwoElementList :: LispExpr -> Bool
notTwoElementList (LispList list) = length list /= 2
notTwoElementList _ =  False

-- function creation
lispLambdaArgs = ["args", "&rest", "body"]
lispLambda = do [(LispList args), _, (LispList body)] <- getSymbols ["args", "body"]
                let newFn = do evalBody <- mapM eval body
                               return $ last evalBody
                return $ LispLambda newFn (map show args)

lispFunArgs = ["name", "args", "&rest", "body"]
lispFun = do [(LispSymbol name), (LispList args), (LispList body)] <- getSymbols  ["name", "args", "body"]
             let newFn = do evalBody <- mapM eval body
                            return $ last evalBody
             let lambda = LispFunc newFn name (map show args)
             updateSymbolInParent name lambda
             return lambda


-- Special form setq
lispSetArgs = ["symbol", "value"]
lispSet = do [(LispSymbol s), e] <- getSymbols lispSetArgs
             eval_e <- eval e
             updateSymbolInParent s eval_e
             return eval_e

-- If Conditional, cond can easily replace it
lispIfArgs = ["condition", "expr1", "expr2"]
lispIf = do [condExpr, expr1, expr2] <- getSymbols lispIfArgs
            eval_cond <- eval condExpr
            if (notNil eval_cond) then eval expr1
                                        else eval expr2
    where notNil (LispSymbol val) = false /= val

lispArithmetic f  = do (LispList args) <- getSymbol "..."
                       lispBinary f args

lispBinary :: (Integer->Integer->Integer) -> [LispExpr] -> LispResult
lispBinary op args = do return $ foldl1 (lispBinaryAux op) args
	where lispBinaryAux op (LispInt i) (LispInt j) = LispInt (i `op` j)


-- Symbol table
initialCtx = Ctx (Map.fromList 
                         [
                          (true, LispSymbol true),
                          (false, LispSymbol false),
			  ("()", LispSymbol "()"),
			  ("quote", LispSpecial lispQuote ["arg"]),
			  ("atom", LispFunc lispAtom "atom" ["&rest", "..."]), 
			  ("eq", LispFunc lispEq "eq" ["&rest", "..."]),
			  ("car", LispFunc lispCar "car" ["&rest", "..."]),
			  ("cdr", LispFunc lispCdr "cdr" ["&rest", "..."]),
                          ("cons", LispFunc lispCons "cons" ["&rest", "..."]),
			  ("cond", LispSpecial lispCond ["&rest", "..."]),
			  ("lambda", LispSpecial lispLambda lispLambdaArgs),
                          ("defun", LispSpecial lispFun lispFunArgs),
                          ("setq", LispSpecial lispSet lispSetArgs),
                          ("+", LispFunc (lispArithmetic (+)) "+" ["&rest", "..."]),
			  ("-", LispFunc (lispArithmetic (-)) "-" ["&rest", "..."]),
			  ("*", LispFunc (lispArithmetic (*)) "*" ["&rest", "..."]),
			  ("/", LispFunc (lispArithmetic div) "/" ["&rest", "..."]),
			  ("if", LispSpecial lispIf lispIfArgs) --redundant
                         ]) Nothing

