module Expressions where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error

-- S-expressions
data LispExpr =  Blank |
                 LispInt Integer |
	         LispSymbol String |
	         LispLambda LispResult FunctionSignature |
	         LispSpecial LispResult FunctionSignature |
	         LispFunc LispResult FunctionName FunctionSignature |
	         LispList [LispExpr] 
   
type FunctionName = String
type FunctionSignature = [String]
type SymbolTable = Map.Map String LispExpr

-- Context (scope) in which expressions are be evaluated
data Context = Ctx { contextSymbols :: SymbolTable, parentContext :: (Maybe Context) }

-- IO Monad + error handling in String form, IO is the inner monad
type LispError = ErrorT String IO
-- State monad holds a context as the state, the error monad as an inner monad and an evaluation result
type LispResult = StateT Context LispError LispExpr

-- Helper functions for context manipulation
updateSymbol symbol value = modify (\(Ctx sym_table parentCtx) -> (Ctx (Map.insert symbol value sym_table)) parentCtx)

updateSymbolInParent symbol value = modify (\(Ctx sym_table parent_ctx)->(Ctx sym_table (updatedCtx parent_ctx)))
    where updatedCtx (Just (Ctx sym_table ctx)) = (Just (Ctx (Map.insert symbol value sym_table) ctx))
          updatedCtx Nothing = Nothing
    
pushContext ctx = Ctx Map.empty (Just ctx)
popContext ctx@(Ctx _ Nothing) = ctx
popContext (Ctx _ (Just parentCtx)) = parentCtx

-- Printing s-expressions
instance Show LispExpr where
        show Blank = ""
	show (LispInt x) = show x
	show (LispSymbol x) = x
	show (LispLambda _ sig) = "<lambda (" ++ (unwords sig) ++ ")>"
	show (LispFunc _ name sig) = "<function "  ++ (show name) ++ " (" ++ (unwords sig) ++ ")>" 
	show (LispSpecial _ _) = "<special-form>"
	show (LispList x) = "(" ++ unwords (map show x) ++ ")"

instance Eq LispExpr where
    (LispInt a) == (LispInt b) = a == b
    (LispSymbol a) == (LispSymbol b) = a == b
    (LispList a) == (LispList b) = a == b
    _ == _ = False
