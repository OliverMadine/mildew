{-# LANGUAGE TypeFamilies #-}

module DebugUtils where

import Combinators ( Combinator(..), AnyCombinator(AnyCombinator) )
import Monitoring ( combinatorSize )

returnValue :: Combinator a -> String
returnValue Pure = "purely correct"
returnValue Empty = "empty"
returnValue Satisfy = "char"
returnValue Chr = "char"
returnValue Item = "char"
returnValue Str = "string"
returnValue (Atomic c) = returnValue c
returnValue (LookAhead c) = returnValue c
returnValue (c :*>: c') = returnValue c'
returnValue (c :<*: c') = returnValue c
returnValue (Fmap c) = "functionally correct"

printCombinator :: Show a => Combinator a -> IO ()
printCombinator c = do
  putStrLn "\nCombinator: "
  print c
  putStrLn "\nSize: "
  print $ combinatorSize c
  putStrLn "\nReturn Value: "
  print $ returnValue c