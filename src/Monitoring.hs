{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Monitoring where

import Combinators ( Combinator(..), AnyCombinator(AnyCombinator) )
import Test.Tasty.QuickCheck ( generate, Gen, Arbitrary(arbitrary) )
import Control.Monad ( replicateM )

import Graphics.Rendering.Chart.Easy
    ( opaque,
      line,
      setColors,
      layout_title,
      plot,
      blue,
      (.=),
      Default(def), layout_x_axis, layout_y_axis, laxis_title )
import Graphics.Rendering.Chart.Backend.Cairo ( toFile )
import Data.List (group, sort)


combinatorSize :: Combinator a -> Int
combinatorSize Pure = 1
combinatorSize Empty = 1
combinatorSize Satisfy = 1
combinatorSize Chr = 1
combinatorSize Item = 1
combinatorSize Str = 1
combinatorSize (Atomic c) = 1 + combinatorSize c
combinatorSize (LookAhead c) = 1 + combinatorSize c
combinatorSize (AnyCombinator c :*> c') = combinatorSize c + combinatorSize c'
combinatorSize (c :<* AnyCombinator c') = combinatorSize c + combinatorSize c'
combinatorSize (Fmap (AnyCombinator c)) = 1 + combinatorSize c
combinatorSize (Some c) = 1 + combinatorSize c
combinatorSize (Many c) = 1 + combinatorSize c
combinatorSize (Choose c c') = combinatorSize c + combinatorSize c'

sampleSizes :: Int -> Gen [Int]
sampleSizes totalSamples = do
    combinators <- replicateM totalSamples (arbitrary :: Gen (Combinator AnyCombinator))
    pure $ map combinatorSize combinators

frequencyDistribution :: [Int] -> [(Int, Int)]
frequencyDistribution = map (\l -> (head l, length l)) . group . sort

plotCombinatorDistribution :: FilePath -> [Int] -> IO ()
plotCombinatorDistribution filePath xs = do
  let freqDist = frequencyDistribution xs
  let values = map fst freqDist
  let frequencies = map snd freqDist

  toFile def filePath $ do
    layout_title .= "Combinator Size Distribution"
    layout_x_axis . laxis_title .= "Combinator Frequency"
    layout_y_axis . laxis_title .= "Combinator Size"
    setColors [opaque blue]
    plot (line "Combinator Size Distribution" [zip values frequencies])
