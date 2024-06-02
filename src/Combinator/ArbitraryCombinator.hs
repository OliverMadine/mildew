{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs        #-}

module Combinator.ArbitraryCombinator where

import           Combinator.Combinator
import           Combinator.GenCombinator
import qualified Test.Tasty.QuickCheck    as QC

class ArbitraryCombinator t where
  arbitrary :: GenCombinator t

advancingGenericCombinators :: [GenCombinator (Combinator a)]
advancingGenericCombinators =
  [ arbitraryUnary Atomic
  , arbitraryUnary Fmap
  , arbitraryBinaryEitherAdvancing Then
  , arbitraryBinaryEitherAdvancing Before
  ]

nonAdvancingGenericCombinators :: [GenCombinator (Combinator a)]
-- HACK: We cannot generate successful inputs for partial consumption in the lookahead case
nonAdvancingGenericCombinators = [ arbitraryUnary (LookAhead . Atomic)]

genericCombinator :: GenCombinator (Combinator a)
genericCombinator = oneof advancingGenericCombinators
-- selectCombinator advancingGenericCombinators nonAdvancingGenericCombinators

advancingListCombinators :: (Show a, QC.Arbitrary a) => [GenCombinator (Combinator [a])]
advancingListCombinators = withAdvancing (arbitraryUnary Some) : advancingGenericCombinators

nonAdvancingListCombinators :: (Show a, QC.Arbitrary a) => [GenCombinator (Combinator [a])]
nonAdvancingListCombinators = withAdvancing (arbitraryUnary Many) : nonAdvancingGenericCombinators

listCombinator :: (Show a, QC.Arbitrary a) => GenCombinator (Combinator [a])
listCombinator = oneof advancingListCombinators
-- listCombinator = selectCombinator advancingListCombinators nonAdvancingListCombinators

nonAdvancingGenericLeafs :: [GenCombinator (Combinator a)]
nonAdvancingGenericLeafs = [ pure Pure ]

-- HACK: we don't have any generic leafs that are advancing right now
genericLeaf :: GenCombinator (Combinator a)
genericLeaf = selectCombinator [pure (Then (AnyCombinator Chr) Pure)] nonAdvancingGenericLeafs

-- HACK: item is considered non-advancing because it consumes any input so we don't want to include
-- it in repeating combinators. Slightly too restrictive but ok for now
nonAdvancingCharLeafs :: [GenCombinator (Combinator Char)]
nonAdvancingCharLeafs = pure Item : nonAdvancingGenericLeafs

charLeaf :: GenCombinator (Combinator Char)
charLeaf = selectCombinator [ pure Satisfy, pure Chr ] nonAdvancingGenericLeafs

stringLeaf :: GenCombinator (Combinator String)
stringLeaf = selectCombinator [ pure Str ] nonAdvancingGenericLeafs

instance {-# OVERLAPS #-} (Show a, QC.Arbitrary a) => ArbitraryCombinator (Combinator [a]) where
  arbitrary :: QC.Arbitrary a => GenCombinator (Combinator [a])
  arbitrary = chooseLeafOrCombinator genericLeaf listCombinator

instance ArbitraryCombinator (Combinator a) where
  arbitrary :: GenCombinator (Combinator a)
  arbitrary = chooseLeafOrCombinator genericLeaf genericCombinator

instance {-# OVERLAPS #-} ArbitraryCombinator (Combinator Char) where
  arbitrary :: GenCombinator (Combinator Char)
  arbitrary = chooseLeafOrCombinator charLeaf genericCombinator

instance {-# OVERLAPS #-} ArbitraryCombinator (Combinator String) where
  arbitrary :: GenCombinator (Combinator String)
  arbitrary = chooseLeafOrCombinator stringLeaf listCombinator

instance ArbitraryCombinator AnyCombinator where
  arbitrary :: GenCombinator AnyCombinator
  arbitrary = oneof
    [ AnyCombinator <$> (arbitrary :: GenCombinator (Combinator Char))
    , AnyCombinator <$> (arbitrary :: GenCombinator (Combinator String))
    , AnyCombinator <$> (arbitrary :: GenCombinator (Combinator Int))
    , AnyCombinator <$> (arbitrary :: GenCombinator (Combinator Bool))
    , AnyCombinator <$> (arbitrary :: GenCombinator (Combinator (Maybe Int)))
    , AnyCombinator <$> (arbitrary :: GenCombinator (Combinator [Int]))
    ]

arbitraryBinaryEitherAdvancing :: (ArbitraryCombinator a, ArbitraryCombinator b) => (a -> b -> t) -> GenCombinator t
arbitraryBinaryEitherAdvancing f = oneof
  [ scaleBinary f arbitrary (withoutAdvancing arbitrary)
  , scaleBinary f (withoutAdvancing arbitrary) arbitrary
  ]

arbitraryBinary :: (ArbitraryCombinator a, ArbitraryCombinator b) => (a -> b -> t) -> GenCombinator t
arbitraryBinary f = scaleBinary f arbitrary arbitrary

arbitraryUnary :: ArbitraryCombinator a => (a -> b) -> GenCombinator b
arbitraryUnary f = f <$> scale pred arbitrary
