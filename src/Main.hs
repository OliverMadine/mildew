{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Combinator.GenCombinator as Combinator
import           Compiler.Gigaparsec      hiding (Failure, Success)
import           Monitoring
import           Parser.ArbitraryParser
import           Parser.Parser
import           Parser.ParserTestCase    hiding (Failure, Success)
import           Test.Tasty
import           Test.Tasty.QuickCheck    hiding (Failure, Success)
import           Text.Gigaparsec          hiding (result)
import           Utils.Debug

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- Combinator.generate (sampleSizes 100000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

testParse :: (Show a, Eq a) => Parser a -> [Char] -> Result String a -> IO Bool
testParse parser input expected = do
  let actual = parse @String (compile parser) input
  let isExpectedResult = case actual of
        (Failure _)   -> isFailure expected
        r@(Success _) -> expected == r
  if isExpectedResult
    then do
      -- printParser parser
      -- printExpected expected
      -- printInput input
      -- printActual actual
      pure True
    else do
      printParser parser
      printExpected expected
      printInput input
      printActual actual
      pure False


prop_no_crash :: Property
prop_no_crash = ioProperty $ do
  ParserTestCase { parser, cases } <- generate (arbitrary :: Gen (ParserTestCase String))
  -- ParserTestCase { parser, cases } <- generate testCase
  results <- mapM (uncurry (testParse parser)) cases
  pure $ and results

main :: IO ()
main = do
  defaultMain $ localOption (QuickCheckTests 10000000) $ testProperty "Main" prop_no_crash

