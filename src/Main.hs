module Main where

import Test.Tasty.QuickCheck ( Arbitrary(arbitrary), Gen, generate )
import Combinators ( Combinator )
import DebugUtils ( printCombinator )
import Control.Monad ( replicateM )
import Monitoring ( combinatorSize, sampleSizes, plotCombinatorDistribution )

main :: IO ()
main = do
    sizes <- generate (sampleSizes 10000)
    plotCombinatorDistribution "combinator-distribution.png" sizes

