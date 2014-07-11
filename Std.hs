module Std where

import Expressions
import Eval
import qualified Data.Map as Map
import Control.Monad.State

lispArithmetic f  = do (LispList args) <- getSymbol "..."
                       lispBinary f args

lispBinary :: (Integer->Integer->Integer) -> [LispExpr] -> LispResult
lispBinary op args = do return $ foldl1 (lispBinaryAux op) args
	where lispBinaryAux op (LispInt i) (LispInt j) = LispInt (i `op` j)

-- Equality
lispEq = do (LispList args) <- getSymbol "..."
            return $ foldl1 (\(LispInt a) (LispInt b) -> LispInt(if a == b then 1 else 0)) args

-- Set modifies context
lispSetArgs = ["symbol", "value"]
lispSet = do [(LispSymbol s), e] <- getSymbols lispSetArgs
             eval_e <- eval e
             updateSymbolInParent s eval_e
             return eval_e

-- If Conditional
lispIfArgs = ["condition", "expr1", "expr2"]
lispIf = do [condExpr, expr1, expr2] <- getSymbols lispIfArgs
            eval_cond <- eval condExpr
            if (0 `notEqual` eval_cond) then eval expr1
                                        else eval expr2
    where notEqual val1 (LispInt val2) = val1 /= val2

-- Function creation
lispFnArgs = ["args", "..."]
lispFn = do [(LispList args), (LispList body)] <- getSymbols lispFnArgs
            let newFn = do evalBody <- mapM eval body
                           return $ last evalBody
            return $ LispFunc newFn (map (\(LispSymbol arg)->arg) args)

-- Symbol table
initialCtx = Ctx (Map.fromList 
                        [("+", LispFunc (lispArithmetic (+)) ["..."]),
			   ("-", LispFunc (lispArithmetic (-)) ["..."]),
			   ("*", LispFunc (lispArithmetic (*)) ["..."]),
			   ("/", LispFunc (lispArithmetic div) ["..."]),
                           ("eq", LispFunc lispEq ["..."]),
			   ("setf", LispSpecial lispSet lispSetArgs),
                           ("if", LispSpecial lispIf lispIfArgs),
                           ("defun", LispSpecial lispFn lispFnArgs )
			  ]) Nothing

-- Helper
getSymbol sym = eval $ (LispSymbol sym)
getSymbols syms = mapM getSymbol syms
