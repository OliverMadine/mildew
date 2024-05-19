{-# LANGUAGE TypeApplications #-}
module Main where

import Test.Tasty.QuickCheck hiding (Success, Failure)
import Combinators
import Monitoring
import Compiler.Gigaparsec
import Text.Gigaparsec
import Test.Tasty

plotSampleSizes :: IO ()
plotSampleSizes = do
    sizes <- generate (sampleSizes 10000)
    plotCombinatorDistribution "combinator-distribution.png" sizes

prop_no_crash :: Combinator String -> String -> Property
prop_no_crash combinator input = ioProperty $ do
    parser <- generate (compile combinator)
    case parse @String parser input of
        Failure _ -> pure True
        Success _ -> pure True

main :: IO ()
main = defaultMain $
       localOption (QuickCheckTests 1000000) $
       testProperty "Test No Crash" prop_no_crash