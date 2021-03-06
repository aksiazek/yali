module Eval where

import Expressions
import qualified Data.Map as Map
import Data.List
import Control.Monad.State
import Control.Monad.Error

eval :: LispExpr -> LispResult
eval Blank = return Blank
eval (LispInt n) = return (LispInt n)
eval (LispLambda f args) = return (LispLambda f args)
eval (LispFunc f name args) = return (LispFunc f name args)
eval (LispSpecial f args) = return (LispSpecial f args)
eval (LispSymbol s) = do context <- get
                         lookupSymbol context
    where lookupSymbol (Ctx sym_table parentCtx) =
              if s `Map.member` sym_table == True
              then return (sym_table Map.! s)
              else case parentCtx of
                     Nothing -> throwError ("Symbol " ++ s ++ " is unbound.")
                     (Just parent) -> lookupSymbol parent
              
eval (LispList []) = return (LispList [])
eval (LispList (x:xs)) = eval x >>= apply
	where apply (LispSpecial f expectedArgs) = apply' expectedArgs xs f
	      apply (LispLambda f expectedArgs) = do args <- mapM eval xs
                                                     apply' expectedArgs args f
	      apply (LispFunc f _ expectedArgs) = apply (LispLambda f expectedArgs)
              apply _ = throwError "Illegal function call"
              apply' expectedArgs args f = do modify pushContext
                                              applyArgsToContext expectedArgs args
                                              result <- f
                                              modify popContext
                                              return result
              applyArgsToContext ("&rest":[]) args = throwError "&rest without rest variable"
              applyArgsToContext ("&rest":restArg:[]) args = do updateSymbol restArg (LispList args)
              applyArgsToContext ("&rest":_:garbage) args = throwError $ "found garbage in lambda list" ++ (show garbage) 
              applyArgsToContext (earg:expectedArgs) (arg:args) = do updateSymbol earg arg
                                                                     applyArgsToContext expectedArgs args
              applyArgsToContext [] [] = return ()
              applyArgsToContext _ _ = throwError "Invalid number of arguments"
