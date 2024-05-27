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
  , arbitraryBinaryEitherAdvancing (:*>)
  , arbitraryBinaryEitherAdvancing (:<*)
  -- TODO: once we have better input generation then only one side
  -- of the alternative needs to maintain the advancing property.
  -- We need to make sure that the side at least one advancing side
  -- doesn't fail in the advancing case
  , arbitraryBinary Alternative
  ]

nonAdvancingGenericCombinators :: [GenCombinator (Combinator a)]
nonAdvancingGenericCombinators = [ arbitraryUnary LookAhead]

genericCombinator :: GenCombinator (Combinator a)
genericCombinator = selectCombinator advancingGenericCombinators nonAdvancingGenericCombinators

advancingListCombinators :: (Show a, QC.Arbitrary a) => [GenCombinator (Combinator [a])]
advancingListCombinators = withAdvancing (arbitraryUnary Some) : advancingGenericCombinators

nonAdvancingListCombinators :: (Show a, QC.Arbitrary a) => [GenCombinator (Combinator [a])]
nonAdvancingListCombinators = withAdvancing (arbitraryUnary Many) : nonAdvancingGenericCombinators

listCombinator :: (Show a, QC.Arbitrary a) => GenCombinator (Combinator [a])
listCombinator = selectCombinator advancingListCombinators nonAdvancingListCombinators

nonAdvancingGenericLeafs :: [GenCombinator (Combinator a)]
nonAdvancingGenericLeafs = [ pure Pure ]

-- HACK: we don't have any generic leafs that are advancing right now
genericLeaf :: GenCombinator (Combinator a)
genericLeaf = selectCombinator [pure (AnyCombinator Item :*> Pure)] nonAdvancingGenericLeafs

charLeaf :: GenCombinator (Combinator Char)
charLeaf = selectCombinator [ pure Satisfy, pure Chr, pure Item ] nonAdvancingGenericLeafs

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
