{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Gigaparsec where

import           Control.Applicative
import           Debug.Trace
import           Generators.Combinators
import           Test.Tasty.QuickCheck
import           Text.Gigaparsec
import           Text.Gigaparsec.Char

-- TODO: compile from Parser type
-- compile :: Parser t -> Parsec t

traceAndShow :: Show b => b -> b
traceAndShow a = traceShow a a

compile :: (Arbitrary t, Show t) => Combinator t -> Gen (Parsec t)
compile Pure                       = pure <$> undefined
compile Satisfy                    = satisfy <$> arbitrary
compile Chr                        = char <$> arbitrary
compile Item                       = pure item
compile Str                        = string <$> arbitrary
compile (Atomic c)                 = atomic <$> compile c
compile (LookAhead c)              = lookAhead <$> compile c
compile (Fmap (AnyCombinator c))   = liftA2 (<$>) arbitrary (compile c)
compile (Some c)                   = some <$> compile c
compile (Many c)                   = many <$> compile c
compile ((AnyCombinator c) :*> c') = liftA2 (*>) (compile c) (compile c')
compile (c :<* (AnyCombinator c')) = liftA2 (<*) (compile c) (compile c')
compile (Alternative c c')         = liftA2 (<|>) (compile c) (compile c')

