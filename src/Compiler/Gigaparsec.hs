{-# LANGUAGE TypeFamilies #-}

module Compiler.Gigaparsec where

import Combinators ( Combinator (..), AnyCombinator (AnyCombinator) )
import Text.Gigaparsec ( Parsec, empty, lookAhead, atomic, (<|>) )
import Text.Gigaparsec.Char ( satisfy, char, item, string )
import Test.Tasty.QuickCheck (Arbitrary(arbitrary), Gen, CoArbitrary)
import Control.Applicative ( Alternative(empty, some, many) )

compile :: Arbitrary t => Combinator t -> Gen (Parsec t)
compile Pure = pure <$> arbitrary
compile Empty = pure empty
compile Satisfy = satisfy <$> arbitrary
compile Chr = char <$> arbitrary
compile Item = pure item
compile Str = string <$> arbitrary
compile (Atomic c) = atomic <$> compile c
compile (LookAhead c) = lookAhead <$> compile c
compile ((AnyCombinator c) :*> c') = compile c *> compile c'
compile (c :<* (AnyCombinator c')) = compile c <* compile c'
compile (Fmap (AnyCombinator c)) = (<*>) . pure <$> arbitrary <*> compile c
compile (Some c) = some <$> compile c
compile (Many c) = many <$> compile c
compile (Choose c c') = (<|>) <$> compile c <*> compile c'
