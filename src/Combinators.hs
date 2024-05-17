{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Combinators where

import Test.Tasty.QuickCheck ( oneof, Arbitrary(arbitrary), Gen, frequency )
import Control.Applicative (Applicative(liftA2))
import Type.Reflection ( Typeable )
import Data.Data ( Typeable )
import Debug.Trace ( trace )

data AnyCombinator = forall a. (Typeable a) => AnyCombinator (Combinator a)
deriving instance Show AnyCombinator

data Combinator a where
  Pure      :: Combinator a
  Satisfy   :: Combinator Char
  Empty     :: Combinator a
  Chr       :: Combinator Char
  Item      :: Combinator Char
  Str       :: Combinator String
  Atomic    :: Combinator a -> Combinator a
  LookAhead :: Combinator a -> Combinator a
  (:*>:)    :: AnyCombinator -> Combinator a -> Combinator a
  (:<*:)    :: Combinator a -> AnyCombinator -> Combinator a
  Fmap      :: AnyCombinator -> Combinator a
deriving instance Show (Combinator a)

artistryWrapper :: Gen (Combinator a) -> Gen (Combinator a)
artistryWrapper specificArbitrary = oneof
  [ pure Pure
  , pure Empty
  , Atomic <$> specificArbitrary
  , LookAhead <$> specificArbitrary
  , liftA2 (:*>:) arbitrary specificArbitrary
  , liftA2 (:<*:) specificArbitrary arbitrary
  , Fmap <$> arbitrary
  ]

instance Arbitrary (Combinator a) where
  arbitrary :: Gen (Combinator a)
  arbitrary = artistryWrapper arbitrary

instance {-# OVERLAPS #-} Arbitrary (Combinator Char) where
  arbitrary :: Gen (Combinator Char)
  arbitrary = oneof
    [ pure Satisfy
    , pure Chr
    , pure Item
    , artistryWrapper arbitrary
    ]

instance {-# OVERLAPS #-} Arbitrary (Combinator String) where
  arbitrary :: Gen (Combinator String)
  arbitrary = oneof
    [ pure Str
    , artistryWrapper arbitrary
    ]

data AnyMaybe = forall a. (Typeable a) => AnyMaybe (Maybe a)

instance Arbitrary AnyCombinator where
  arbitrary :: Gen AnyCombinator
  arbitrary = oneof
    [ AnyCombinator <$> (arbitrary :: Gen (Combinator Char))
    , AnyCombinator <$> (arbitrary :: Gen (Combinator String))
    , AnyCombinator <$> (arbitrary :: Gen (Combinator Int))
    , AnyCombinator <$> (arbitrary :: Gen (Combinator Bool))
    , AnyCombinator <$> (arbitrary :: Gen (Combinator (Maybe AnyMaybe)))
    ]

instance Arbitrary AnyMaybe where
    arbitrary :: Gen AnyMaybe
    arbitrary = oneof
        [ AnyMaybe <$> (arbitrary :: Gen (Maybe Char))
        , AnyMaybe <$> (arbitrary :: Gen (Maybe String))
        , AnyMaybe <$> (arbitrary :: Gen (Maybe Int))
        , AnyMaybe <$> (arbitrary :: Gen (Maybe Bool))
        ]
