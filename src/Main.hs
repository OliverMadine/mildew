{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Combinator.ArbitraryCombinator as Combinator
import           Combinator.Combinator
import           Combinator.GenCombinator       hiding (generate)
import qualified Combinator.GenCombinator       as Combinator
import           Compiler.Gigaparsec            hiding (Failure, Success)
import           Monitoring
import           Parser.ArbitraryParser
import           Parser.GenParserTestCase       hiding (Failure, Success)
import           Test.Tasty
import           Test.Tasty.QuickCheck          hiding (Failure, Success,
                                                 arbitrary, generate)
import           Text.Gigaparsec                hiding (result)
import           Utils.Debug

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- Combinator.generate (sampleSizes 100000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

prop_no_crash :: Property
prop_no_crash = ioProperty $ do
  -- combinator <- Combinator.generate (Combinator.arbitrary :: GenCombinator (Combinator String))
  ParserTestCase { parser, input, result = expected } <- generate (arbitrary :: GenParserTestCase (ParserTestCase String))
  let actual = parse @String (compile parser) input
  let isExpectedResult = case actual of
        (Failure _)   -> isFailure expected
        r@(Success _) -> expected == r
  if isExpectedResult
    then pure True
    else do
      -- printCombinator combinator
      printParser parser
      printExpected expected
      printInput input
      printActual actual
      pure False

main :: IO ()
main = do
  defaultMain $ localOption (QuickCheckTests 1000000) $ testProperty "Main" prop_no_crash

