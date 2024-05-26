{-# LANGUAGE TypeFamilies #-}

module Utils.Debug where

import           Combinators.Combinators
import           Monitoring

printCombinator :: Show a => Combinator a -> IO ()
printCombinator c = do
  putStrLn "\nCombinator: "
  print c
  putStrLn "\nSize: "
  print $ combinatorSize c
