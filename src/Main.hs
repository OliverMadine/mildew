{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import           Combinator.Combinator    as Combinator
import qualified Combinator.GenCombinator as Combinator
import           Compiler.Gigaparsec      hiding (Failure, Success)
import           Control.Monad            (zipWithM)
import           Monitoring
import           Parser.ArbitraryParser
import           Parser.Parser
import           Parser.ParserTestCase    hiding (Failure, Success)
import           Test.Tasty
import           Test.Tasty.QuickCheck    hiding (Failure, Success)
import           Text.Gigaparsec          hiding (result)
import           Utils.Debug
import qualified Combinator.ArbitraryCombinator as Combinator
import Control.Monad.Trans.State

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- Combinator.generate (sampleSizes 1000000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

isExpectedResult :: Eq a => Result String a -> Result String a -> Bool
isExpectedResult expected (Failure _)   = isFailure expected
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
  defaultMain $ localOption (QuickCheckTests 10000) $ testProperty "Combinator Distribution" prop_comb
