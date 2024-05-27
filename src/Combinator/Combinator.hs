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

module Combinator.Combinator where

import           Test.Tasty.QuickCheck
import           Utils.Print

data Combinator a where
  Pure        :: Combinator a
  Satisfy     :: Combinator Char
  Chr         :: Combinator Char
  Item        :: Combinator Char
  Str         :: Combinator String
  Atomic      :: Combinator a -> Combinator a
  LookAhead   :: Combinator a -> Combinator a
  Then        :: AnyCombinator -> Combinator a -> Combinator a
  Before      :: Combinator a -> AnyCombinator -> Combinator a
  Fmap        :: AnyCombinator -> Combinator a
  Some        :: (Show a, Arbitrary a) => Combinator a -> Combinator [a]
  Many        :: (Show a, Arbitrary a) => Combinator a -> Combinator [a]
  Alternative :: Combinator a -> Combinator a -> Combinator a

data AnyCombinator = forall a. (Show a, CoArbitrary a, Arbitrary a, Eq a) => AnyCombinator (Combinator a)
deriving instance Show AnyCombinator

instance Show (Combinator a) where
  show :: Combinator a -> String
  show Pure                          = "pure"
  show Satisfy                       = "satisfy"
  show Chr                           = "chr"
  show Item                          = "item"
  show Str                           = "str"
  show (Atomic c)                    = "atomic" ++ parensShow c
  show (LookAhead c)                 = "lookAhead" ++ parensShow c
  show (Fmap (AnyCombinator c))      = "fmap (" ++ show c ++ ")"
  show (Some c)                      = "some" ++ parensShow c
  show (Many c)                      = "many" ++ parensShow c
  show (Then (AnyCombinator c) c')   = parensShow c ++ " *> " ++ parensShow c'
  show (Before c (AnyCombinator c')) = parensShow c ++ " <* " ++ parensShow c'
  show (Alternative c c')            = parensShow c ++ " <|> " ++ parensShow c'
