{-# LANGUAGE TypeApplications #-}

module Main where

import           Combinators.Combinators
import           Combinators.GenCombinator
import           Compiler.Gigaparsec       hiding (Failure, Success)
import           Monitoring
import           Test.Tasty
import qualified Test.Tasty.QuickCheck     as QC
import           Test.Tasty.QuickCheck     hiding (Failure, Success, arbitrary,
                                            generate)
import           Text.Gigaparsec
import           Utils.Debug

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- generate (sampleSizes 10000000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

prop_no_crash :: [Char] -> Property
prop_no_crash input = ioProperty $ do
  combinator <- generate (arbitrary :: GenCombinator (Combinator Char))
  parser <- QC.generate (compile combinator)
  printCombinator combinator
  print input
  let result = parse @String parser input
  case result of
    Failure _ -> pure True
    Success _ -> pure True

main :: IO ()
main = defaultMain $
  localOption (QuickCheckTests 1000000) $
  testProperty "Test No Crash" prop_no_crash
