{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Combinator.ArbitraryCombinator where

import           Combinator.Combinator
import           Combinator.GenCombinator
import qualified Test.Tasty.QuickCheck    as QC

class ArbitraryCombinator t where
  arbitrary :: GenCombinator t

advancingGenericCombinators :: ArbitraryCombinator (Combinator a) => [GenCombinator (Combinator a)]
advancingGenericCombinators =
  [ arbitraryUnary Atomic
  , arbitraryUnary Fmap
  , scaleBinary Then arbitrary (withoutAdvancing arbitrary)
  , scaleBinary Before arbitrary (withoutAdvancing arbitrary)
  ]

nonAdvancingGenericCombinators :: ArbitraryCombinator (Combinator a) => [GenCombinator (Combinator a)]
nonAdvancingGenericCombinators = [ arbitraryUnary LookAhead ]

genericCombinator :: ArbitraryCombinator (Combinator a) => GenCombinator (Combinator a)
genericCombinator = selectCombinator advancingGenericCombinators nonAdvancingGenericCombinators

advancingListCombinators :: (Show a, QC.Arbitrary a, ArbitraryCombinator (Combinator [a]), ArbitraryCombinator (Combinator a)) => [GenCombinator (Combinator [a])]
-- We cannot generate successful inputs for partial consumption in the lookAhead case
advancingListCombinators = withAdvancing (arbitraryUnary (Some . Atomic)) : advancingGenericCombinators

nonAdvancingListCombinators :: (Show a, QC.Arbitrary a, ArbitraryCombinator (Combinator [a])) => [GenCombinator (Combinator [a])]
nonAdvancingListCombinators = nonAdvancingGenericCombinators
-- nonAdvancingListCombinators = withAdvancing (arbitraryUnary Many) nonAdvancingGenericCombinators

listCombinator :: (Show a, QC.Arbitrary a, ArbitraryCombinator (Combinator [a]), ArbitraryCombinator (Combinator a)) => GenCombinator (Combinator [a])
listCombinator = selectCombinator advancingListCombinators nonAdvancingListCombinators

nonAdvancingGenericLeafs :: [GenCombinator (Combinator a)]
nonAdvancingGenericLeafs = [ pure Pure ]

-- There does not exist any advancing leaf with a generic type. Chr *> Pure is used
-- to create a parser that both consumes input and return a generically types result
genericLeaf :: ArbitraryCombinator (Combinator a) => GenCombinator (Combinator a)
genericLeaf = selectCombinator [pure (Then (AnyCombinator Chr) Pure)] nonAdvancingGenericLeafs

-- HACK: item is considered non-advancing because it consumes any input so we don't want to include
-- it in repeating combinators. Slightly too restrictive but ok for now
nonAdvancingCharLeafs :: [GenCombinator (Combinator Char)]
nonAdvancingCharLeafs = pure Item : nonAdvancingGenericLeafs

charLeaf :: GenCombinator (Combinator Char)
charLeaf = selectCombinator [ pure Satisfy, pure Chr ] nonAdvancingGenericLeafs
-- charLeaf = selectCombinator [ pure Satisfy, pure Chr ] nonAdvancingCharLeafs

stringLeaf :: GenCombinator (Combinator String)
stringLeaf = selectCombinator [ pure Str ] nonAdvancingGenericLeafs

instance {-# OVERLAPPABLE #-} ArbitraryCombinator (Combinator a) where
  arbitrary :: GenCombinator (Combinator a)
  arbitrary = chooseLeafOrCombinator genericLeaf genericCombinator
  
instance {-# OVERLAPS #-} (Show a, QC.Arbitrary a) => ArbitraryCombinator (Combinator [a]) where
  arbitrary :: QC.Arbitrary a => GenCombinator (Combinator [a])
  arbitrary = chooseLeafOrCombinator genericLeaf listCombinator

instance {-# OVERLAPPING #-} ArbitraryCombinator (Combinator Char) where
  arbitrary :: GenCombinator (Combinator Char)
  arbitrary = chooseLeafOrCombinator charLeaf genericCombinator

instance {-# OVERLAPPING #-} ArbitraryCombinator (Combinator String) where
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

arbitraryBinary :: (ArbitraryCombinator a, ArbitraryCombinator b) => (a -> b -> t) -> GenCombinator t
arbitraryBinary f = scaleBinary f arbitrary arbitrary

arbitraryUnary :: ArbitraryCombinator a => (a -> b) -> GenCombinator b
arbitraryUnary f = f <$> scale pred arbitrary
