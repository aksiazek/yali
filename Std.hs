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
             -- lispBinaryAux op _ _ = throwError "Error in binary operation."

-- Equality
lispEq = do (LispList args) <- getSymbol "..."
            return $ foldl1 isEqual args

isEqual :: LispExpr -> LispExpr -> LispExpr
isEqual (LispSymbol a) (LispSymbol b) = LispSymbol(if a == b then "t" else "nil")
isEqual (LispInt a) (LispInt b) = LispSymbol(if a == b then "t" else "nil")
isEqual (LispList a) (LispList b) = LispSymbol(if (null a) && (null b) then "t" else "nil")
isEqual _ _ = LispSymbol "nil"

-- Quoting
singleArg = ["value"]
lispQuote = getSymbol "value"

lispAtomArgs = ["value"]
lispAtom = do [expr] <- getSymbols lispAtomArgs
              eval_e <- eval expr
              if(isAtom eval_e) then return $ LispSymbol "t"
              else return $ LispSymbol "nil"
                  where isAtom (LispList list) = null list
                        isAtom  _ = True

lispCar = do expr <- getSymbol "..."
             eval_e <- eval expr
             if(isAtom eval_e) then return $ LispSymbol "t" --throwError "bad"
             else return $ LispSymbol "nil"
                 where isAtom (LispList list) = null list
                       isAtom  _ = True


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

-- Function creation
lispFnArgs = ["args", "..."]
lispFn = do [(LispList args), (LispList body)] <- getSymbols lispFnArgs
        
            let newFn = do evalBody <- mapM eval body
                           return $ last evalBody
            return $ LispLambda newFn (map show args)

-- Symbol table
initialCtx = Ctx (Map.fromList 
                         [
                          ("t", LispSymbol "t"),
                          ("nil", LispSymbol "nil"),
			  ("quote", LispSpecial lispQuote singleArg),
			  ("atom", LispFunc lispAtom "atom" lispAtomArgs), 
			  ("eq", LispFunc lispEq "eq" ["..."]),
			  ("car", LispFunc lispCar "car" ["..."]),
			  ("+", LispFunc (lispArithmetic (+)) "+" ["..."]),
			  ("-", LispFunc (lispArithmetic (-)) "-" ["..."]),
			  ("*", LispFunc (lispArithmetic (*)) "*" ["..."]),
			  ("/", LispFunc (lispArithmetic div) "/" ["..."]),
			  ("setq", LispSpecial lispSet lispSetArgs),
                          ("if", LispSpecial lispIf lispIfArgs),
                          ("lambda", LispSpecial lispFn lispFnArgs )
                         --("defun", LispSpecial lispFn lispFnArgs )
			 ]) Nothing

-- Helper
getSymbol sym = eval $ (LispSymbol sym)
getSymbols syms = mapM getSymbol syms
