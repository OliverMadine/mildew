{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser.ParserTestCase where

import           Control.Monad.Trans.State
import           Parser.Parser
import qualified Test.Tasty.QuickCheck     as QC
import           Text.Gigaparsec

deriving instance Functor (Result e)

type GenParser t = StateT GenParserState QC.Gen t

-- Constraints on the following n characters
data GenParserState = GenParserState {
  follows   :: [CharConstraint],
  precludes :: [Char]
} deriving (Show)

type TestCase a = (String, Result String a)

data ParserTestCases a = ParserTestCases
  { parser :: Parser a
  , cases  :: [TestCase a]
  }

isFailure :: Result String a -> Bool
isFailure (Failure _) = True
isFailure _           = False
