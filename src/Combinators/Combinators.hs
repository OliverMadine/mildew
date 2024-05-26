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

module Combinators.Combinators where

import           Test.Tasty.QuickCheck
import           Utils.Print

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
  Some        :: Arbitrary a => Combinator a -> Combinator [a]
  Many        :: Arbitrary a => Combinator a -> Combinator [a]
  Alternative :: Combinator a -> Combinator a -> Combinator a

data AnyCombinator = forall a. (CoArbitrary a, Arbitrary a) => AnyCombinator (Combinator a)
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
  show (Fmap (AnyCombinator c))   = "fmap (" ++ show c ++ ")"
  show (Some c)                   = "some" ++ parensShow c
  show (Many c)                   = "many" ++ parensShow c
  show ((AnyCombinator c) :*> c') = parensShow c ++ " *> " ++ parensShow c'
  show (c :<* (AnyCombinator c')) = parensShow c ++ " <* " ++ parensShow c'
  show (Alternative c c')         = parensShow c ++ " <|> " ++ parensShow c'
