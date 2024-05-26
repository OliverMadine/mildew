{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE UndecidableInstances #-}

module Generators.GenCombinator where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Test.Tasty.QuickCheck     as QC

combinatorsPerLeaf :: Int
combinatorsPerLeaf = 4

type GenCombinator t = StateT GenCombinatorState QC.Gen t

class ArbitraryCombinator t where
  arbitrary :: GenCombinator t

newtype GenCombinatorState = GenCombinatorState
  { advancing :: Bool }

initState :: GenCombinatorState
initState = GenCombinatorState
  { advancing = False }

modifyAdvancing :: Bool -> GenCombinator t -> GenCombinator t
modifyAdvancing newAdvancing gen = do
  currentAdvancing <- advancing <$> get
  modify (\s -> s { advancing = newAdvancing })
  result <- gen
  modify (\s -> s { advancing = currentAdvancing })
  pure result

withAdvancing :: GenCombinator t -> GenCombinator t
withAdvancing = modifyAdvancing True

withoutAdvancing :: GenCombinator t -> GenCombinator t
withoutAdvancing = modifyAdvancing False

oneof :: [GenCombinator t] -> GenCombinator t
oneof [] = error "ArbitraryCombinators.oneof used with empty list"
oneof combinators = lift (QC.chooseInt (0, length combinators - 1)) >>= (combinators !!)

scale :: (Int -> Int) -> GenCombinator t -> GenCombinator t
scale f = mapStateT (QC.scale f)

chooseLeafOrCombinator :: GenCombinator t -> GenCombinator t -> GenCombinator t
chooseLeafOrCombinator leaf combinator = do
  size <- lift QC.getSize
  n <- lift (QC.chooseInt (0, combinatorsPerLeaf))
  if size <= 0 || n == 0 then leaf else combinator

selectCombinator :: [GenCombinator a] -> [GenCombinator a] -> GenCombinator a
selectCombinator advancingCombinators nonAdvancingCombinators = do
  GenCombinatorState { advancing } <- get
  if advancing
    then oneof advancingCombinators
    else oneof $ nonAdvancingCombinators ++ advancingCombinators

generate :: GenCombinator t -> IO t
generate gen = QC.generate $ evalStateT gen initState
