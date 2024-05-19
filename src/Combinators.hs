{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Combinators where

import           Control.Applicative
import           Test.Tasty.QuickCheck

-- TODO: better way to to handle frequency between leafs, branches, and wrappers

data AnyCombinator = forall a. (CoArbitrary a, Arbitrary a, Show a) => AnyCombinator (Combinator a)
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
  (:*>)     :: AnyCombinator -> Combinator a -> Combinator a
  (:<*)     :: Combinator a -> AnyCombinator -> Combinator a
  Fmap      :: AnyCombinator -> Combinator a
  Some      :: (Arbitrary a, Show a) => Combinator a -> Combinator [a]
  Many      :: (Arbitrary a, Show a) => Combinator a -> Combinator [a]
  Choose    :: Combinator a -> Combinator a -> Combinator a

instance Show (Combinator a) where
  show :: Combinator a -> String
  show Pure                       = "pure"
  show Satisfy                    = "satisfy"
  show Empty                      = "empty"
  show Chr                        = "chr"
  show Item                       = "item"
  show Str                        = "str"
  show (Atomic c)                 = "atomic" ++ parensShow c
  show (LookAhead c)              = "lookAhead" ++ parensShow c
  show ((AnyCombinator c) :*> c') = parensShow c ++ " *> " ++ parensShow c'
  show (c :<* (AnyCombinator c')) = parensShow c ++ " <* " ++ parensShow c'
  show (Fmap (AnyCombinator c))   = "fmap (" ++ show c ++ ")"
  show (Some c)                   = "some" ++ parensShow c
  show (Many c)                   = "many" ++ parensShow c
  show (Choose c c')              = parensShow c ++ " <|> " ++ parensShow c'

genericLeaf :: Gen (Combinator a)
genericLeaf = oneof
  [ pure Pure
  , pure Empty
  ]

genericCombinator :: Gen (Combinator a)
genericCombinator = sized $ \n -> frequency
  [ (6, genericLeaf)
  , (n, arbitraryUnary Atomic)
  , (n, arbitraryUnary LookAhead)
  , (n, arbitraryBinary (:*>))
  , (n, arbitraryBinary (:<*))
  , (n, arbitraryUnary Fmap)
  , (n, arbitraryBinary Choose)
  ]

genericList :: (Arbitrary a, Show a) =>  Gen (Combinator [a])
genericList = sized $ \n -> frequency
  [ (2, arbitraryUnary Some)
  , (2, arbitraryUnary Many)
  , (n, genericCombinator)
  ]

instance {-# OVERLAPS #-} (Arbitrary a, Show a) => Arbitrary (Combinator [a]) where
  arbitrary :: (Arbitrary a, Show a) => Gen (Combinator [a])
  arbitrary = genericList

instance Arbitrary (Combinator a) where
  arbitrary :: Gen (Combinator a)
  arbitrary = genericCombinator

instance {-# OVERLAPS #-} Arbitrary (Combinator Char) where
  arbitrary :: Gen (Combinator Char)
  arbitrary = sized $ \n -> frequency
    [ (2, pure Satisfy)
    , (2, pure Chr)
    , (2, pure Item)
    , (n, genericCombinator)
    ]

instance {-# OVERLAPS #-} Arbitrary (Combinator String) where
  arbitrary :: Gen (Combinator String)
  arbitrary = sized $ \n -> frequency
    [ (8, pure Str)
    , (n, genericList)
    ]

instance Arbitrary AnyCombinator where
  arbitrary :: Gen AnyCombinator
  arbitrary = oneof
    [ AnyCombinator <$> (arbitrary :: Gen (Combinator Char))
    , AnyCombinator <$> (arbitrary :: Gen (Combinator String))
    , AnyCombinator <$> (arbitrary :: Gen (Combinator Int))
    , AnyCombinator <$> (arbitrary :: Gen (Combinator Bool))
    , AnyCombinator <$> (arbitrary :: Gen (Combinator AnyMaybe))
    , AnyCombinator <$> (arbitrary :: Gen (Combinator AnyList))
    ]

data AnyMaybe = forall a. (CoArbitrary a, Show a) => AnyMaybe (Maybe a)
deriving instance Show AnyMaybe

instance Arbitrary AnyMaybe where
  arbitrary :: Gen AnyMaybe
  arbitrary = oneof
    [ AnyMaybe <$> (arbitrary :: Gen (Maybe Char))
    , AnyMaybe <$> (arbitrary :: Gen (Maybe String))
    , AnyMaybe <$> (arbitrary :: Gen (Maybe Int))
    , AnyMaybe <$> (arbitrary :: Gen (Maybe Bool))
    ]

instance CoArbitrary AnyMaybe where
  coarbitrary :: AnyMaybe -> Gen b -> Gen b
  coarbitrary (AnyMaybe x) = coarbitrary x

data AnyList = forall a. (CoArbitrary a, Show a) => AnyList [a]
deriving instance Show AnyList

instance Arbitrary AnyList where
  arbitrary :: Gen AnyList
  arbitrary = oneof
    [ AnyList <$> (arbitrary :: Gen [Char])
    , AnyList <$> (arbitrary :: Gen [String])
    , AnyList <$> (arbitrary :: Gen [Int])
    , AnyList <$> (arbitrary :: Gen [Bool])
    ]

instance CoArbitrary AnyList where
  coarbitrary :: AnyList -> Gen b -> Gen b
  coarbitrary (AnyList xs) = coarbitrary xs

parensShow :: Show a => a -> String
parensShow s = '(' : show s ++ ")"

arbitraryBinary :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> Gen c
arbitraryBinary f = liftA2 f (scale (`div` 2) arbitrary) (scale (`div` 2) arbitrary)

arbitraryUnary :: Arbitrary a => (a -> b) -> Gen b
arbitraryUnary f = f <$> scale pred arbitrary
