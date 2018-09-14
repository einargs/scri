{-# LANGUAGE MultiWayIf #-}

module Lib
    ( parseStdIn
    ) where

import GhcWrap
import Scri.Ast
import qualified Scri.Token as T
import Lexer
import Parser
import Control.Monad.IO.Class
import System.Environment
import qualified Data.List as List


parseStdIn :: IO ()
parseStdIn = do
  putStrLn "hey"
  args <- getArgs
  let isArg optStr = optStr `elem` args
  s <- getContents
  print s
  {-let useThisParser (optList, _) = any isArg optList
  let maybeSelectedChoice =
        List.find useThisParser
          [ (["--lexTokens", "-t"], onlyLexTokens)
          , (["--parse", "-p"], printParse)
          ]
  case maybeSelectedChoice of
    Just (_, choice) -> choice s
    _ -> return ()-}
  --putStrLn $ either id id $ onlyLexTokens s
  if | isArg "--lexTokens" -> 
       case runAlex s loop of
         (Right tokens) -> mapM_ (print) tokens
         (Left errorMsg) -> putStrLn $ "Error: " ++ errorMsg
     | otherwise ->
       let result = parse s :: Either String Script
       in print result
  where
    printParse s =
      let result = parse s :: Either String Script
      in print result
    onlyLexTokens s = do
      let res = runAlex s loop
      putStrLn $ case res of
        Left errStr -> errStr
        Right tokens -> unlines $ map show tokens
    loop = do
      token <- alexMonadScan
      if token == T.EOFToken
        then return [token]
        else do
          rest <- loop
          return (token:rest)

