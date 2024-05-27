{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Parser.GenParserTestCase where

import qualified Combinator.GenCombinator  as Combinator
import           Control.Monad.Trans.State
import           Parser.Parser
import qualified Test.Tasty.QuickCheck     as QC
import           Text.Gigaparsec           hiding (result)

type GenParserTestCase t = StateT GenParserState QC.Gen t

deriving instance Functor (Result e) -- Gigaparsec's result does not have a Functor instance

data ParserTestCase a = ParserTestCase
  { parser :: Parser a
  , input  :: String
  , result :: Result String a
  }

newtype GenParserState = GenParserState
  { followSet :: [String] }

initGenParserState :: GenParserState
initGenParserState = GenParserState { followSet = [] }

isFailure :: Result String a -> Bool
isFailure (Failure _) = True
isFailure _           = False

generate :: GenParserTestCase t -> IO t
generate gen = QC.generate (QC.resize 2 $ evalStateT gen initGenParserState)
