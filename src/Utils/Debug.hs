{-# LANGUAGE TypeFamilies #-}

module Utils.Debug where

import           Combinator.Combinator
import           Monitoring
import           Parser.Parser
import           Text.Gigaparsec

printCombinator :: Show a => Combinator a -> IO ()
printCombinator c = do
  putStrLn "\nCombinator: "
  print c
  putStrLn "\nSize: "
  print $ combinatorSize c

printParser :: Show a => Parser a -> IO ()
printParser p = do
  putStrLn "\nParser: "
  print p

printExpected :: Show a => Result String a -> IO ()
printExpected (Success s) = do
  putStrLn "\nExpected: "
  print s
printExpected (Failure _) = do
  putStrLn "\nExpected: "
  print "Failure"

printActual :: Show a => Result String a -> IO ()
printActual (Success s) = do
  putStrLn "\nActual: "
  print s
printActual (Failure error) = do
  putStrLn "\nActual: "
  putStrLn error

printInput :: Show a => a -> IO ()
printInput input = do
  putStrLn "\nInput: "
  print input
