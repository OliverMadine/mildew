{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Compiler.Gigaparsec      hiding (Failure, Success)
import           Generators.Combinators
import           Generators.GenCombinator
import           Monitoring
import           Parser                   hiding (Failure, Success)
import           Test.Tasty
import qualified Test.Tasty.QuickCheck    as QC
import           Test.Tasty.QuickCheck    hiding (Failure, Success, arbitrary,
                                           generate)
import           Text.Gigaparsec          hiding (result)

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- generate (sampleSizes 100000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

prop_no_crash :: (Arbitrary a, Show a) => Combinator a -> String -> Property
prop_no_crash combinator input = ioProperty $ do
  AnyCombinator combinator <- generate (arbitrary :: GenCombinator AnyCombinator)
  parser <- QC.generate (compile combinator)
  TestCase { parser, input, result } <- generateTestCase combinator
  case parse @String parser input of
    Failure _ -> pure $ isFailure result
    r@(Success _) -> pure (result == r)

main :: IO ()
main = do
  AnyCombinator combinator <- generate (arbitrary :: GenCombinator AnyCombinator)
  defaultMain $ localOption (QuickCheckTests 10000) $ testProperty "Main" (prop_no_crash combinator)

