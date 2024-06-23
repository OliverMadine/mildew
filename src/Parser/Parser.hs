{-# LANGUAGE GADTs        #-}
{-# LANGUAGE InstanceSigs #-}

module Parser.Parser where

import           Utils.Print

data CharConstraint = OneOf [Char] | AnyChar deriving (Eq, Show)

-- A representation of a parser with additional metadata used for our test oracle.
data Parser a where
  Pure        :: Show a => a -> Parser a
  Satisfy     :: [Char] -> (Char -> Bool) -> Parser Char
  Chr         :: Char -> Parser Char
  Item        :: [Char] -> Parser Char
  Str         :: String -> Parser String
  Atomic      :: Parser a -> Parser a
  LookAhead   :: Parser a -> Parser a
  Then        :: Parser b -> Parser a -> Parser a
  Before      :: Parser a -> Parser b -> Parser a
  Fmap        :: (b -> a) -> Parser b -> Parser a
  Some        :: Int -> Parser a -> Parser [a]
  Many        :: Int -> Parser a -> Parser [a]
  Digit       :: [Char] -> Parser Char

instance Show (Parser a) where
  show :: Parser a -> String
  show (Pure a)       = "pure" ++ parensShow a
  show (Satisfy cs p) = "satisfy" ++ "(`elem`" ++ show cs ++ ")"
  show (Chr c)        = "char" ++ parensShow c
  show (Item cs)      = "item" ++ parensShow cs
  show (Str s)        = "string" ++ parensShow s
  show (Atomic c)     = "atomic" ++ parensShow c
  show (LookAhead c)  = "lookAhead" ++ parensShow c
  show (Fmap f c)     = "fmap f (" ++ show c ++ ")"
  show (Some _ c)     = "some" ++ parensShow c
  show (Many _ c)     = "many" ++ parensShow c
  show (Then c c')    = parensShow c ++ " *> " ++ parensShow c'
  show (Before c c')  = parensShow c ++ " <* " ++ parensShow c'
  show (Digit c)      = "digit" ++ parensShow c

inputConstraints :: Parser a -> [Char]
inputConstraints (Pure a)       = []
inputConstraints (Satisfy cs _) = cs
inputConstraints (Chr c)        = [c]
inputConstraints (Item cs)      = cs
inputConstraints (Str s)        = s
inputConstraints (Atomic p)     = inputConstraints p
inputConstraints (LookAhead p)  = []
inputConstraints (Then p p')    = inputConstraints p ++ inputConstraints p'
inputConstraints (Before p p')  = inputConstraints p ++ inputConstraints p'
inputConstraints (Fmap _ p)     = inputConstraints p
inputConstraints (Some _ p)     = inputConstraints p
inputConstraints (Many _ p)     = inputConstraints p
inputConstraints (Digit cs)     = cs
