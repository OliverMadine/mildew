{-# LANGUAGE GADTs        #-}
{-# LANGUAGE InstanceSigs #-}

module Parser.Parser where

import           Utils.Print

data Parser a where
  Pure        :: Show a => a -> Parser a
  Satisfy     :: (Char -> Bool) -> Parser Char
  Chr         :: Char -> Parser Char
  Item        :: Parser Char
  Str         :: String -> Parser String
  Atomic      :: Parser a -> Parser a
  LookAhead   :: Parser a -> Parser a
  Then        :: Parser b -> Parser a -> Parser a
  Before      :: Parser a -> Parser b -> Parser a
  Fmap        :: (b -> a) -> Parser b -> Parser a
  Some        :: Parser a -> Parser [a]
  Many        :: Parser a -> Parser [a]
  Alternative :: Parser a -> Parser a -> Parser a

instance Show (Parser a) where
  show :: Parser a -> String
  show (Pure a)           = "pure" ++ parensShow a
  show (Satisfy p)        = "satisfy"
  show (Chr c)            = "chr" ++ parensShow c
  show Item               = "item"
  show (Str s)            = "str" ++ parensShow s
  show (Atomic c)         = "atomic" ++ parensShow c
  show (LookAhead c)      = "lookAhead" ++ parensShow c
  show (Fmap f c)         = "fmap (" ++ show c ++ ")"
  show (Some c)           = "some" ++ parensShow c
  show (Many c)           = "many" ++ parensShow c
  show (Then c c')        = parensShow c ++ " *> " ++ parensShow c'
  show (Before c c')      = parensShow c ++ " <* " ++ parensShow c'
  show (Alternative c c') = parensShow c ++ " <|> " ++ parensShow c'
