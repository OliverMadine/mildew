{-# LANGUAGE TypeApplications #-}

module Main where

import           Combinators
import           Compiler.Gigaparsec   hiding (Failure, Success)
import           DebugUtils
import           Monitoring
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (Failure, Success)
import           Text.Gigaparsec

plotSampleSizes :: IO ()
plotSampleSizes = do
    sizes <- generate (sampleSizes 10000000)
    plotCombinatorDistribution "combinator-distribution.png" sizes

prop_no_crash :: Combinator String -> String -> Property
prop_no_crash combinator input = ioProperty $ do
    parser <- generate (compile combinator)
    printCombinator combinator
    print input
    case parse @String parser input of
        Failure _ -> pure True
        Success _ -> pure True

main :: IO ()
main = defaultMain $
        localOption (QuickCheckTests 1000000) $
        testProperty "Test No Crash" prop_no_crash
