{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

module Combinators where

import           Control.Applicative
import           Test.Tasty.QuickCheck

-- I need to support
-- e.g some(atomic(char 'a'))
-- and some(atomic(empty))
-- not some(atomic(empty) <|> lookAhead 'a')
-- not some(atomic(pure 1))
-- not some(many(char 'a'))

data Combinator a where
  Pure        :: Combinator a
  Satisfy     :: Combinator Char
  Chr         :: Combinator Char
  Item        :: Combinator Char
  Str         :: Combinator String
  Atomic      :: Combinator a -> Combinator a
  LookAhead   :: Combinator a -> Combinator a
  (:*>)       :: AnyCombinator -> Combinator a -> Combinator a
  (:<*)       :: Combinator a -> AnyCombinator -> Combinator a
  Fmap        :: AnyCombinator -> Combinator a
  Some        :: (Arbitrary a, Show a) => Combinator a -> Combinator [a]
  Many        :: (Arbitrary a, Show a) => Combinator a -> Combinator [a]
  Alternative :: Combinator a -> Combinator a -> Combinator a

data AnyCombinator = forall a. (CoArbitrary a, Arbitrary a, Show a) => AnyCombinator (Combinator a)
deriving instance Show AnyCombinator

instance Show (Combinator a) where
  show :: Combinator a -> String
  show Pure                       = "pure"
  show Satisfy                    = "satisfy"
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
  show (Alternative c c')         = parensShow c ++ " <|> " ++ parensShow c'

leafs :: [Gen (Combinator a)]
leafs = [ pure Pure ]

charLeafs :: [Gen (Combinator Char)]
charLeafs = [ pure Satisfy, pure Chr, pure Item ] ++ leafs

stringLeafs :: [Gen (Combinator String)]
stringLeafs = pure Str : leafs

combinators :: [Gen (Combinator a)]
combinators =
  [ arbitraryUnary Atomic
  , arbitraryUnary LookAhead
  , arbitraryBinary (:*>)
  , arbitraryBinary (:<*)
  , arbitraryUnary Fmap
  , arbitraryBinary Alternative
  ]

listCombinators :: (Arbitrary a, Show a) => [Gen (Combinator [a])]
listCombinators = [ arbitraryUnary Some, arbitraryUnary Many ] ++ combinators

instance {-# OVERLAPS #-} (Arbitrary a, Show a) => Arbitrary (Combinator [a]) where
  arbitrary :: (Arbitrary a, Show a) => Gen (Combinator [a])
  arbitrary = sizedOneOf leafs listCombinators

instance Arbitrary (Combinator a) where
  arbitrary :: Gen (Combinator a)
  arbitrary = sizedOneOf leafs combinators

instance {-# OVERLAPS #-} Arbitrary (Combinator Char) where
  arbitrary :: Gen (Combinator Char)
  arbitrary = sizedOneOf charLeafs combinators

instance {-# OVERLAPS #-} Arbitrary (Combinator String) where
  arbitrary :: Gen (Combinator String)
  arbitrary = sizedOneOf stringLeafs listCombinators

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
arbitraryBinary f = scale pred $ liftA2 f (scale (`div` 2) arbitrary) (scale (`div` 2) arbitrary)

arbitraryUnary :: Arbitrary a => (a -> b) -> Gen b
arbitraryUnary f = f <$> scale pred arbitrary

sizedOneOf :: [Gen a] -> [Gen a] -> Gen a
sizedOneOf leafs combinators = sized gen
  where
    gen 0 = oneof leafs
    gen n = oneof (leafs ++ combinators)
