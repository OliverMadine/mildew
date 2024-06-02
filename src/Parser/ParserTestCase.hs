{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser.ParserTestCase where

import qualified Combinator.GenCombinator  as Combinator
import           Control.Monad.Trans.State
import           Parser.Parser
import           Text.Gigaparsec           hiding (result)

deriving instance Functor (Result e)

data CharConstraint = OneOf [Char] | AnyChar deriving (Eq, Show)

data ParserTestCase a = ParserTestCase
  { parser :: Parser a
  , cases :: [(String, Result String a)]
  }

isFailure :: Result String a -> Bool
isFailure (Failure _) = True
isFailure _           = False
