{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser.ParserTestCase where

import qualified Combinator.GenCombinator as Combinator
import           Parser.Parser
import           Text.Gigaparsec

deriving instance Functor (Result e)

data CharConstraint = OneOf [Char] | AnyChar deriving (Eq, Show)

type TestCases a = [(String, Result String a)]

data ParserTestCase a = ParserTestCase
  { parser :: Parser a
  , cases  :: TestCases a
  }

isFailure :: Result String a -> Bool
isFailure (Failure _) = True
isFailure _           = False
