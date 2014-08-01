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
lispQuote = getSymbol "thing" >>= return

lispAtom = do arg <- getSymbol "object"  
              return $ LispSymbol (if isAtom arg then true else false)

isAtom :: LispExpr -> Bool
isAtom (LispList _) = False
isAtom _ = True

lispEqArgs = ["expr1","expr2"]
lispEq = do [expr1,expr2] <- getSymbols lispEqArgs
            return $ isEqual expr1 expr2

isEqual :: LispExpr -> LispExpr -> LispExpr
isEqual a b
    | a == b = LispSymbol true
    | otherwise = LispSymbol false

lispCar = do list <- getSymbol "list"  
             action list
                 where action arg
                           | arg == (LispList []) = return $ (LispSymbol nothing)
                           | isAtom $ arg = throwError "Argument is not of type list" 
                           | otherwise = return $ car arg

lispCdr = do list <- getSymbol "list"  
             action list
                 where action arg
                           | arg == (LispList []) = return $ arg
                           | isAtom arg = throwError "Argument is not of type list" 
                           | otherwise = return $ cdr arg

car :: LispExpr -> LispExpr
car (LispList list) = head list
car _ = (LispSymbol nothing)

cdr :: LispExpr -> LispExpr
cdr (LispList list) = (LispList (tail list))
cdr _ = (LispSymbol nothing)

notNothing :: LispExpr -> Bool
notNothing = (/= (LispSymbol nothing))
            
lispConsArgs = ["expr1","expr2"]
lispCons = do [expr1,expr2] <- getSymbols lispConsArgs 
              action expr1 expr2
                  where action expr1 expr2
                            | (isAtom $ expr2) && (notNothing $ expr2)
                                = throwError "Cannot construct list"
                            | otherwise = return $ splice expr1 expr2  

splice :: LispExpr -> LispExpr -> LispExpr
splice elem (LispSymbol nothing) = (LispList [elem])
splice elem (LispList list) = (LispList (elem:list))
splice _ _ = (LispSymbol false)

lispCond = do (LispList args) <- getSymbol "clauses"  
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
lispLambda = do [(LispList args), (LispList body)] <- getSymbols ["args", "body"]
                let newFn = do evalBody <- mapM eval body
                               return $ last evalBody
                return $ LispLambda newFn (map show args)

lispFunArgs = ["name", "args", "&rest", "body"]
lispFun = do [(LispSymbol name), (LispList args), (LispList body)] <- getSymbols  ["name", "args", "body"]
             let newFn = do evalBody <- mapM eval body
                            return $ last evalBody
                 lambda = LispFunc newFn name (map show args)
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

lispArithmetic f  = do (LispList args) <- getSymbol "args"
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
			  ("quote", LispSpecial lispQuote ["thing"]),
			  ("atom", LispFunc lispAtom "atom" ["object"]), 
			  ("eq", LispFunc lispEq "eq" lispEqArgs),
			  ("car", LispFunc lispCar "car" ["list"]),
			  ("cdr", LispFunc lispCdr "cdr" ["list"]),
                          ("cons", LispFunc lispCons "cons" ["expr1", "expr2"]),
			  ("cond", LispSpecial lispCond ["&rest", "clauses"]),
			  ("lambda", LispSpecial lispLambda lispLambdaArgs),
                          ("defun", LispSpecial lispFun lispFunArgs),
                          ("setq", LispSpecial lispSet lispSetArgs),
                          ("+", LispFunc (lispArithmetic (+)) "+" ["&rest", "args"]),
			  ("-", LispFunc (lispArithmetic (-)) "-" ["&rest", "args"]),
			  ("*", LispFunc (lispArithmetic (*)) "*" ["&rest", "args"]),
			  ("/", LispFunc (lispArithmetic div) "/" ["&rest", "args"]),
			  ("if", LispSpecial lispIf lispIfArgs) --redundant
                         ]) Nothing

