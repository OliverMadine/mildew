{-# LANGUAGE TypeFamilies #-}

module Compiler.Gigaparsec where

import           Combinators
import           Control.Applicative
import           Test.Tasty.QuickCheck
import           Text.Gigaparsec
import           Text.Gigaparsec.Char

-- TODO: compile from Parser type
-- compile :: Parser t -> Parsec t

compile :: Arbitrary t => Combinator t -> Gen (Parsec t)
compile Pure                       = pure <$> arbitrary
compile Empty                      = pure empty
compile Satisfy                    = satisfy <$> arbitrary
compile Chr                        = char <$> arbitrary
compile Item                       = pure item
compile Str                        = string <$> arbitrary
compile (Atomic c)                 = atomic <$> compile c
compile (LookAhead c)              = lookAhead <$> compile c
compile ((AnyCombinator c) :*> c') = compile c *> compile c'
compile (c :<* (AnyCombinator c')) = compile c <* compile c'
compile (Fmap (AnyCombinator c))   = liftA2 (<$>) arbitrary (compile c)
compile (Some c)                   = some <$> compile c
compile (Many c)                   = many <$> compile c
compile (Choose c c')              = liftA2 (<|>) (compile c) (compile c')

