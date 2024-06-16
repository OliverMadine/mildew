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
import Control.Monad (zipWithM)

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- Combinator.generate (sampleSizes 100000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

isExpectedResult :: Eq a => Result String a -> Result String a -> Bool
isExpectedResult expected (Failure _) = isFailure expected
isExpectedResult expected r@(Success _) = expected == r

testGigaparsec :: Parsec Int -> String -> Result String Int -> Bool
testGigaparsec parser input expected =
  isExpectedResult expected (parse @String parser input)

prop_model_parser :: Property
prop_model_parser = ioProperty $ do
  ParserTestCases { parser, cases } <- generate (arbitrary :: Gen (ParserTestCases Int))
  let gigaparsecParser = compile parser
      results = map (uncurry (testGigaparsec gigaparsecParser)) cases
  pure $ and results

main :: IO ()
main = do
  defaultMain $ localOption (QuickCheckTests 10000000) $ testProperty "Main" prop_model_parser
