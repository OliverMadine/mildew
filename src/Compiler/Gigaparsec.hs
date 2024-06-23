{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Compiler.Gigaparsec where

import           Control.Applicative
import           Parser.Parser
import           Text.Gigaparsec
import           Text.Gigaparsec.Char

compile :: Parser t -> Parsec t
compile (Pure a)      = pure a
compile (Satisfy _ p) = satisfy p
compile (Chr c)       = char c
compile (Item _)      = item
compile (Str s)       = string s
compile (Atomic c)    = atomic (compile c)
compile (LookAhead c) = lookAhead (compile c)
compile (Fmap f c)    = f <$> compile c
compile (Some _ c)    = some (compile c)
compile (Many _ c)    = many (compile c)
compile (Then c c')   = compile c *> compile c'
compile (Before c c') = compile c <* compile c'
compile (Digit _)     = digit
