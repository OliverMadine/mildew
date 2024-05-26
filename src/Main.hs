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

plotSampleSizes :: IO ()
plotSampleSizes = do
  sizes <- generate (sampleSizes 10000000)
  plotCombinatorDistribution "combinator-distribution.png" sizes

prop_no_crash :: (Arbitrary a, Show a) => Combinator a -> String -> Property
prop_no_crash combinator input = ioProperty $ do
  parser <- QC.generate (compile combinator)
  case parse @String parser input of
    Failure _ -> pure True
    Success _ -> pure True

main :: IO ()
main = do
  combinators <- replicateM 100000 $ generate (arbitrary :: GenCombinator AnyCombinator)
  -- sequence_ [printCombinator c | AnyCombinator c <- combinators]
  let tests = [createTest n c | (n, c) <- zip [0..] combinators]
  defaultMain $ testGroup "Test No Crash" tests

createTest :: Int -> AnyCombinator -> TestTree
createTest n (AnyCombinator c) = localOption (QuickCheckTests 10000) $ testProperty (show n) (prop_no_crash c)
