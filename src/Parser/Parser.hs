{-# LANGUAGE GADTs        #-}
{-# LANGUAGE InstanceSigs #-}

module Parser.Parser where

import           Utils.Print

-- A representation of a parser with additional metadata used for our test oracle.
data Parser a where
  Pure        :: Show a => a -> Parser a
  Satisfy     :: [Char] -> (Char -> Bool) -> Parser Char
  Chr         :: Char -> Parser Char
  Item        :: Parser Char
  Str         :: String -> Parser String
  Atomic      :: Parser a -> Parser a
  LookAhead   :: Parser a -> Parser a
  Then        :: Parser b -> Parser a -> Parser a
  Before      :: Parser a -> Parser b -> Parser a
  Fmap        :: (b -> a) -> Parser b -> Parser a
  Some        :: Int -> Parser a -> Parser [a]
  Many        :: Int -> Parser a -> Parser [a]

instance Show (Parser a) where
  show :: Parser a -> String
  show (Pure a)       = "pure" ++ parensShow a
  show (Satisfy cs p) = "satisfy" ++ parensShow cs
  show (Chr c)        = "chr" ++ parensShow c
  show Item           = "item"
  show (Str s)        = "str" ++ parensShow s
  show (Atomic c)     = "atomic" ++ parensShow c
  show (LookAhead c)  = "lookAhead" ++ parensShow c
  show (Fmap f c)     = "fmap (" ++ show c ++ ")"
  show (Some _ c)     = "some" ++ parensShow c
  show (Many _ c)     = "many" ++ parensShow c
  show (Then c c')    = parensShow c ++ " *> " ++ parensShow c'
  show (Before c c')  = parensShow c ++ " <* " ++ parensShow c'
