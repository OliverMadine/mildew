{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Parser.GenParserTestCase where

import qualified Combinator.GenCombinator  as Combinator
import           Control.Monad.Trans.State
import           Parser.Parser
import qualified Test.Tasty.QuickCheck     as QC
import           Text.Gigaparsec           hiding (result)

type GenParserTestCase t = StateT GenParserState QC.Gen t

deriving instance Functor (Result e)

data ParserTestCase a = ParserTestCase
  { parser :: Parser a
  , input  :: String -- TODO: this needs to be multiple input to consider cases like some(item) *> char 'a'
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
generate gen = QC.generate (QC.resize 4 $ evalStateT gen initGenParserState)
