module Parser where

import           Compiler.Gigaparsec
import           Generators.Combinators
import           Generators.GenCombinator
import qualified Test.Tasty.QuickCheck    as QC
import           Text.Gigaparsec          hiding (result)

data TestCase a = TestCase
  { parser :: Parsec a
  , input  :: String
  , result :: Result String a
  }

isFailure :: Result String a -> Bool
isFailure (Failure _) = True
isFailure _           = False

-- Generate combinator and traverse to create parser a test case.
-- Construct input and select values as to predict the test case result.
generateTestCase :: Combinator a -> IO (TestCase a)
generateTestCase combinator = do
  -- TODO
  pure $ TestCase { parser = undefined, input = "", result = Success undefined }
