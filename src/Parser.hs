module Parser where

import Combinators
import Test.Tasty.QuickCheck

data Parser a = Parser

-- We cannot accurately predict the expected error.
-- It is not strongly defined and is subject to change
data ExpectedResult a = Success a | Failure

data TestCase a = ParserInfo
  { parser :: Parser a
  , input  :: String
  , result :: ExpectedResult a
  }


-- Generate combinator and traverse to create parser a test case.
-- Construct input and select values as to predict the test case result.
generateTestCase :: Gen (TestCase a)
generateTestCase = undefined
