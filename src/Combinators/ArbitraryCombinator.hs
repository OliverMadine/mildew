{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Combinators.ArbitraryCombinator where

import           Combinators.Combinators
import           Combinators.GenCombinator
import           Control.Applicative
import qualified Test.Tasty.QuickCheck     as QC

leafs :: [GenCombinator (Combinator a)]
leafs = [ pure Pure ]

charLeafs :: [GenCombinator (Combinator Char)]
charLeafs = [ pure Satisfy, pure Chr, pure Item ] ++ leafs

stringLeafs :: [GenCombinator (Combinator String)]
stringLeafs = pure Str : leafs

combinators :: [GenCombinator (Combinator a)]
combinators =
  [ arbitraryUnary Atomic
  , arbitraryUnary LookAhead
  , arbitraryUnary Fmap
  , arbitraryBinary (:*>)
  , arbitraryBinary (:<*)
  , arbitraryBinary Alternative
  ]

arbitrarySome :: QC.Arbitrary a => GenCombinator (Combinator [a])
arbitrarySome = withAdvancing $ arbitraryUnary Some

listCombinators :: QC.Arbitrary a => [GenCombinator (Combinator [a])]
listCombinators = [ arbitraryUnary Some, arbitraryUnary Many ] ++ combinators

instance {-# OVERLAPS #-} ArbitraryCombinator (Combinator [a]) where
  arbitrary :: GenCombinator (Combinator [a])
  arbitrary = sizedOneOf leafs combinators

instance ArbitraryCombinator (Combinator a) where
  arbitrary :: GenCombinator (Combinator a)
  arbitrary = undefined

instance {-# OVERLAPS #-} ArbitraryCombinator (Combinator Char) where
  arbitrary :: GenCombinator (Combinator Char)
  arbitrary = undefined

instance {-# OVERLAPS #-} ArbitraryCombinator (Combinator String) where
  arbitrary :: GenCombinator (Combinator String)
  arbitrary = undefined

instance ArbitraryCombinator AnyCombinator where
  arbitrary :: GenCombinator AnyCombinator
  arbitrary = oneof
    [ AnyCombinator <$> (arbitrary :: GenCombinator (Combinator Char))
    , AnyCombinator <$> (arbitrary :: GenCombinator (Combinator String))
    , AnyCombinator <$> (arbitrary :: GenCombinator (Combinator Int))
    , AnyCombinator <$> (arbitrary :: GenCombinator (Combinator Bool))
    , AnyCombinator <$> (arbitrary :: GenCombinator (Combinator AnyMaybe))
    , AnyCombinator <$> (arbitrary :: GenCombinator (Combinator AnyList))
    ]

data AnyMaybe = forall a. (QC.CoArbitrary a, Show a) => AnyMaybe (Maybe a)
deriving instance Show AnyMaybe

instance QC.Arbitrary AnyMaybe where
  arbitrary :: QC.Gen AnyMaybe
  arbitrary = QC.oneof
    [ AnyMaybe <$> (QC.arbitrary :: QC.Gen (Maybe Char))
    , AnyMaybe <$> (QC.arbitrary :: QC.Gen (Maybe String))
    , AnyMaybe <$> (QC.arbitrary :: QC.Gen (Maybe Int))
    , AnyMaybe <$> (QC.arbitrary :: QC.Gen (Maybe Bool))
    ]

instance QC.CoArbitrary AnyMaybe where
  coarbitrary :: AnyMaybe -> QC.Gen b -> QC.Gen b
  coarbitrary (AnyMaybe x) = QC.coarbitrary x

data AnyList = forall a. (QC.CoArbitrary a, Show a) => AnyList [a]
deriving instance Show AnyList

instance QC.Arbitrary AnyList where
  arbitrary :: QC.Gen AnyList
  arbitrary = QC.oneof
    [ AnyList <$> (QC.arbitrary :: QC.Gen [Char])
    , AnyList <$> (QC.arbitrary :: QC.Gen [String])
    , AnyList <$> (QC.arbitrary :: QC.Gen [Int])
    , AnyList <$> (QC.arbitrary :: QC.Gen [Bool])
    ]

instance QC.CoArbitrary AnyList where
  coarbitrary :: AnyList -> QC.Gen b -> QC.Gen b
  coarbitrary (AnyList xs) = QC.coarbitrary xs

arbitraryBinary :: (ArbitraryCombinator a, ArbitraryCombinator b) => (a -> b -> c) -> GenCombinator c
arbitraryBinary f = scale pred $ liftA2 f (scale (`div` 2) arbitrary) (scale (`div` 2) arbitrary)

arbitraryUnary :: ArbitraryCombinator a => (a -> b) -> GenCombinator b
arbitraryUnary f = f <$> scale pred arbitrary
