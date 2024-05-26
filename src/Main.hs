{-# LANGUAGE TypeApplications #-}

module Main where

import           Compiler.Gigaparsec      hiding (Failure, Success)
import           Control.Monad
import           Generators.Combinators
import           Generators.GenCombinator
import           Monitoring
import           Test.Tasty
import qualified Test.Tasty.QuickCheck    as QC
import           Test.Tasty.QuickCheck    hiding (Failure, Success, arbitrary,
                                           generate)
import           Text.Gigaparsec
import           Utils.Debug

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- generate (sampleSizes 100000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

prop_no_crash :: (Arbitrary a, Show a) => Combinator a -> String -> Property
prop_no_crash combinator input = ioProperty $ do
  parser <- QC.generate (compile combinator)
  case parse @String parser input of
    Failure _ -> pure True
    Success _ -> pure True

main :: IO ()
main = do
  -- plotSampleSizes
  combinators <- replicateM 1000 $ generate (arbitrary :: GenCombinator AnyCombinator)
  -- sequence_ [printCombinator c | AnyCombinator c <- combinators]
  let tests = [createTest n c | (n, AnyCombinator c) <- zip [0..] combinators, combinatorSize c > 50]
  defaultMain $ testGroup "Test No Crash" tests

createTest :: (Arbitrary a, Show a) => Int -> Combinator a -> TestTree
createTest n c = localOption (QuickCheckTests 1000) $ testProperty (show n) (withMaxShrinks 1 (withMaxSize 1000 (prop_no_crash c)))
