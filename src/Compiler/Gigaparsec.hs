{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Compiler.Gigaparsec where

import           Control.Applicative
import           Parser.Parser
import           Text.Gigaparsec
import           Text.Gigaparsec.Char

compile :: Parser t -> Parsec t
compile (Pure a)           = pure a
compile (Satisfy p)        = satisfy p
compile (Chr c)            = char c
compile Item               = item
compile (Str s)            = string s
compile (Atomic c)         = atomic (compile c)
compile (LookAhead c)      = lookAhead (compile c)
compile (Fmap f c)         = f <$> compile c
compile (Some c)           = some (compile c)
compile (Many c)           = many (compile c)
compile (Then c c')        = compile c *> compile c'
compile (Before c c')      = compile c <* compile c'
compile (Alternative c c') = compile c <|> compile c'
