module Main where

import Expressions -- for testing
import Parser
import Eval
import Std --(initialCtx)
import System.IO
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error

main = do runErrorT (evalStateT repl initialCtx)
          return ()

repl = do 
  liftIO $ putStr "lisp> "
  liftIO $ hFlush stdout
  x <- liftIO getLine
  when (x /= "(quit)") $ do
	 expr <- parse x 
         evaledExpr <- eval expr
         case (show evaledExpr) of
           "" -> liftIO (putStr $ show evaledExpr)
	   _ -> liftIO $ print evaledExpr
	 repl
         `catchError` (\e -> do liftIO $ putStrLn e
                                repl)
