{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

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

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- Combinator.generate (sampleSizes 100000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

prop_no_crash :: String -> Property
prop_no_crash input = ioProperty $ do
  ParserTestCase { parser, input, result } <- generate (arbitrary :: GenParserTestCase (ParserTestCase String))
  case parse @String (compile parser) input of
    Failure _     -> pure $ isFailure result
    r@(Success _) -> pure (result == r)

main :: IO ()
main = do
  defaultMain $ localOption (QuickCheckTests 10000) $ testProperty "Main" prop_no_crash

