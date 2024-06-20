{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Combinator.ArbitraryCombinator as Combinator
import           Combinator.Combinator          as Combinator
import qualified Combinator.GenCombinator       as Combinator
import           Compiler.Gigaparsec            hiding (Failure, Success)
import           Control.Monad                  (zipWithM)
import           Control.Monad.Trans.State
import           Monitoring
import           Parser.ArbitraryParser
import           Parser.Parser
import           Parser.ParserTestCase          hiding (Failure, Success)
import           Test.Tasty
import           Test.Tasty.QuickCheck          hiding (Failure, Success)
import           Text.Gigaparsec                hiding (result)
import Utils.Debug (printParser, printActual, printExpected, printInput)

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- Combinator.generate (sampleSizes 1000000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

isExpectedResult :: Eq a => Result String a -> Result String a -> Bool
isExpectedResult expected (Failure _)   = isFailure expected
isExpectedResult expected r@(Success _) = expected == r

testGigaparsec :: Parser Int -> String -> Result String Int -> IO Bool
testGigaparsec parser input expected = do
  let gigaparsecParser = compile parser
  case parse @String gigaparsecParser input of
    r@(Failure _)   -> if isFailure expected
      then do
        pure True
      else do
        printInput input
        printExpected expected
        printActual r
        printParser parser
        pure False
    r@(Success _) -> if expected == r
      then pure True
      else do
        printInput input
        printExpected expected
        printActual r
        printParser parser
        pure False

prop_model_parser :: Property
prop_model_parser = ioProperty $ do
  ParserTestCases { parser, cases } <- generate (arbitrary :: Gen (ParserTestCases Int))
  results <- mapM (uncurry (testGigaparsec parser)) cases
  pure $ and results

instance Arbitrary (Combinator a) where
  arbitrary :: Gen (Combinator a)
  arbitrary = Combinator.evalGenCombinator Combinator.arbitrary

-- Define the property using collect
prop_comb :: Combinator Int -> Property
prop_comb comb =
    collect (show comb) $ -- Convert comb to a string to use as a label
    property True

main :: IO ()
main = do
  defaultMain $ localOption (QuickCheckTests 10000) $ testProperty "" prop_model_parser
