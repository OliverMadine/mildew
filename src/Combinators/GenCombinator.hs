{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Combinators.GenCombinator where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Test.Tasty.QuickCheck as QC

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

sized :: (Int -> GenCombinator t) -> GenCombinator t
sized = (lift QC.getSize >>=)

scale :: (Int -> Int) -> GenCombinator t -> GenCombinator t
scale f = mapStateT (QC.scale f)

sizedOneOf :: [GenCombinator t] -> [GenCombinator t] -> GenCombinator t
sizedOneOf leafs combinators = sized gen
  where
    gen 0 = oneof leafs
    gen n = oneof (leafs ++ combinators)

generate :: GenCombinator t -> IO t
generate gen = QC.generate $ evalStateT gen initState
