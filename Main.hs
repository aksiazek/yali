module Main where

import Expressions (Context,LispError)
import Parser (parse)
import Eval (eval)
import Std (initialCtx)
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad.State
import Control.Monad.Error

main = runErrorT (evalStateT repl initialCtx)

repl :: StateT Context LispError ()
repl = do 
  liftIO $ putStr "lisp> "
  liftIO $ hFlush stdout
  x <- liftIO $ getLine `catch` eofHandler
  if (x /= "(quit)") then do
	               expr <- parse x 
                       evaledExpr <- eval expr
                       liftIO $ print evaledExpr
	               repl
                       `catchError` (\e -> do liftIO $ putStrLn e
                                              repl)
  else do liftIO $ putStrLn ""
          return ()

eofHandler e = if isEOFError e then return "(quit)" else ioError e
