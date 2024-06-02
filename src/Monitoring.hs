{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Monitoring where

import           Combinator.ArbitraryCombinator
import           Combinator.Combinator
import           Combinator.GenCombinator
import           Control.Monad
import           Data.List
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

combinatorSize :: Combinator a -> Int
combinatorSize Pure                          = 1
combinatorSize Satisfy                       = 1
combinatorSize Chr                           = 1
combinatorSize Item                          = 1
combinatorSize Str                           = 1
combinatorSize (Atomic c)                    = 1 + combinatorSize c
combinatorSize (LookAhead c)                 = 1 + combinatorSize c
combinatorSize (Fmap (AnyCombinator c))      = 1 + combinatorSize c
combinatorSize (Some c)                      = 1 + combinatorSize c
combinatorSize (Many c)                      = 1 + combinatorSize c
combinatorSize (Then (AnyCombinator c) c')   = combinatorSize c + combinatorSize c'
combinatorSize (Before c (AnyCombinator c')) = combinatorSize c + combinatorSize c'
combinatorSize (Alternative c c')            = combinatorSize c + combinatorSize c'

sampleSizes :: Int -> GenCombinator [Int]
sampleSizes totalSamples = do
  combinators <- replicateM totalSamples (arbitrary :: GenCombinator AnyCombinator)
  pure $ [combinatorSize c | AnyCombinator c <- combinators]

frequencyDistribution :: [Int] -> [(Int, Int)]
frequencyDistribution = map (\l -> (head l, length l)) . group . sort

plotCombinatorDistribution :: FilePath -> [Int] -> IO ()
plotCombinatorDistribution filePath xs = do
  let freqDist = frequencyDistribution xs
  let values = map fst freqDist
  let frequencies = map snd freqDist

  toFile def filePath $ do
    layout_title .= "Combinator Size Distribution"
    layout_y_axis . laxis_title .= "Combinator Frequency"
    layout_x_axis . laxis_title .= "Combinator Size"
    setColors [opaque blue]
    plot (line "Combinator Size Distribution" [zip values frequencies])

