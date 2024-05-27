{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Combinator.GenCombinator as Combinator
import           Compiler.Gigaparsec      hiding (Failure, Success)
import           Monitoring
import           Parser.ArbitraryParser
import           Parser.GenParserTestCase hiding (Failure, Success)
import           Test.Tasty
import           Test.Tasty.QuickCheck    hiding (Failure, Success, arbitrary,
                                           generate)
import           Text.Gigaparsec          hiding (result)
import           Utils.Debug

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- Combinator.generate (sampleSizes 100000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

prop_no_crash :: Property
prop_no_crash = ioProperty $ do
  ParserTestCase { parser, input, result = expected } <- generate (arbitrary :: GenParserTestCase (ParserTestCase Char))
  -- printParser parser
  -- printExpected expected
  -- printInput input
  let actual = parse @String (compile parser) input
  -- printActual actual
  case actual of
    Failure _     -> pure $ isFailure expected
    r@(Success _) -> pure (expected == r)

main :: IO ()
main = do
  defaultMain $ localOption (QuickCheckTests 1000000) $ testProperty "Main" prop_no_crash

