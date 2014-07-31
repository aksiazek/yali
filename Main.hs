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
  when (x /= "(quit)") $ do
	 expr <- parse x 
         evaledExpr <- eval expr
         case (show evaledExpr) of
           "" -> liftIO (putStr $ show evaledExpr)
	   _ -> liftIO $ print evaledExpr
	 repl
         `catchError` (\e -> do liftIO $ putStrLn e
                                repl)

eofHandler e = if isEOFError e then return "(quit)" else ioError e
