{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Combinators where

import Test.Tasty.QuickCheck ( oneof, Arbitrary(arbitrary), Gen, frequency, sized, scale, getSize )
import Control.Applicative (Applicative(liftA2))

data AnyCombinator = forall a. (Show a) => AnyCombinator (Combinator a)
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

instance Show a => Show (Combinator a) where
  show :: Combinator a -> String
  show Pure = "pure"
  show Satisfy = "satisfy"
  show Empty = "empty"
  show Chr = "chr"
  show Item = "item"
  show Str = "str"
  show (Atomic c) = "atomic" ++ parensShow c
  show (LookAhead c) = "lookAhead" ++ parensShow c
  show ((AnyCombinator c) :*>: c') = parensShow c ++ " *> " ++ parensShow c'
  show (c :<*: (AnyCombinator c')) = parensShow c ++ " <* " ++ parensShow c'
  show (Fmap (AnyCombinator c)) = "fmap (" ++ show c ++ ")"

genericLeaf :: Gen (Combinator a)
genericLeaf = oneof
  [ pure Pure
  , pure Empty
  ]

genericCombinator :: Gen (Combinator a)
genericCombinator = sized $ \n -> frequency
    [ (6, genericLeaf)
    , (n, Atomic <$> decrementSize arbitrary)
    , (n, LookAhead <$> decrementSize arbitrary)
    , (n, liftA2 (:*>:) (halfSize arbitrary) (halfSize arbitrary))
    , (n, liftA2 (:<*:) (halfSize arbitrary) (halfSize arbitrary))
    , (n, Fmap <$> decrementSize arbitrary)
    ]

instance Arbitrary (Combinator a) where
  arbitrary :: Gen (Combinator a)
  arbitrary = genericCombinator

instance {-# OVERLAPS #-} Arbitrary (Combinator Char) where
  arbitrary :: Gen (Combinator Char)
  arbitrary = sized $ \n -> frequency
    [ (2, pure Satisfy)
    , (2, pure Chr)
    , (2, pure Item)
    , (n, decrementSize genericCombinator)
    ]

instance {-# OVERLAPS #-} Arbitrary (Combinator String) where
  arbitrary :: Gen (Combinator String)
  arbitrary = sized $ \n -> frequency
    [ (6, pure Str)
    , (n, decrementSize genericCombinator)
    ]

data AnyMaybe = forall a. (Show a) => AnyMaybe (Maybe a)
deriving instance Show AnyMaybe

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

parensShow :: Show a => a -> String
parensShow s = '(' : show s ++ ")"

halfSize :: Gen a -> Gen a
halfSize = scale (`div` 2)

decrementSize :: Gen a -> Gen a
decrementSize = scale pred