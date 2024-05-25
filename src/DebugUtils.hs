{-# LANGUAGE TypeFamilies #-}

module DebugUtils where

import           Combinators.Combinators (AnyCombinator (AnyCombinator),
                                          Combinator (..))
import           Monitoring              (combinatorSize)


printCombinator :: Show a => Combinator a -> IO ()
printCombinator c = do
  putStrLn "\nCombinator: "
  print c
  putStrLn "\nSize: "
  print $ combinatorSize c
