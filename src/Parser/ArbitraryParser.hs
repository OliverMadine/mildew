{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE UndecidableInstances    #-}

module Parser.ArbitraryParser where

import           Combinator.ArbitraryCombinator as Combinator
import           Combinator.Combinator
import           Combinator.GenCombinator       as Combinator
import           Control.Monad.Trans.Class
import           Parser.GenParserTestCase
import qualified Parser.Parser                  as Parser
import           Test.Tasty.QuickCheck          as QC hiding (Success, generate)
import           Text.Gigaparsec                hiding (result)
import           Text.Gigaparsec.Char

class ArbitraryParser t where
  arbitrary :: GenParserTestCase (ParserTestCase t)

instance (Arbitrary a, ArbitraryCombinator (Combinator a), Show a) => ArbitraryParser a where
  arbitrary :: (Arbitrary a, ArbitraryCombinator (Combinator a), Show a) => GenParserTestCase (ParserTestCase a)
  arbitrary = do
    combinator <- lift $ evalGenCombinatorState (Combinator.arbitrary :: GenCombinator (Combinator a))
    arbitraryTestCase combinator

arbitraryTestCase :: (Arbitrary a, Show a) => Combinator a -> GenParserTestCase (ParserTestCase a)
arbitraryTestCase Pure = do
  result <- lift QC.arbitrary
  let parser = Parser.Pure result
  pure ParserTestCase { parser, input = "", result = Success result }


--      (*>)
--      / \
--     /   \
-- char   some
--   'a'    |
--         atomic
--           |
--          string
--            |
--           "abc"
-- testComb :: Combinator [String]
-- testComb = AnyCombinator Chr :*> Some (Atomic Str)

testInstance :: Parsec [String]
testInstance = char 'a' *> some (string "abc")
