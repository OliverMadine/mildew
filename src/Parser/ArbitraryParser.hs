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
import           Combinator.GenCombinator
import           Control.Monad.Trans.Class
import qualified Parser.Parser                  as Parser
import           Parser.ParserTestCase
import           Test.Tasty.QuickCheck          as QC hiding (Failure, Success)
import           Text.Gigaparsec                hiding (result)

instance (Arbitrary a, ArbitraryCombinator (Combinator a), Show a) => Arbitrary (ParserTestCase a) where
  arbitrary :: (Arbitrary a, ArbitraryCombinator (Combinator a), Show a) => Gen (ParserTestCase a)
  arbitrary = do
    combinator <- evalGenCombinatorState Combinator.arbitrary
    arbitraryTestCase combinator

arbitraryTestCase :: (Arbitrary a, Show a) => Combinator a -> Gen (ParserTestCase a)
arbitraryTestCase Pure = do
  a <- QC.arbitrary
  pure ParserTestCase { parser = Parser.Pure a, input = "", result = Success a }
arbitraryTestCase Satisfy = do
  chars <- listOf1 QC.arbitrary
  let p = (`elem` chars)
  c <- QC.oneof $ map pure chars
  pure ParserTestCase { parser = Parser.Satisfy p, input = [c], result = Success c }
arbitraryTestCase Chr = do
  c <- QC.arbitrary
  pure ParserTestCase { parser = Parser.Chr c, input = [c], result = Success c }
arbitraryTestCase Item = do
  c <- QC.arbitrary
  pure ParserTestCase { parser = Parser.Item, input = [c], result = Success c }
arbitraryTestCase Str = do
  s <- listOf1 QC.arbitrary
  pure ParserTestCase { parser = Parser.Str s, input = s, result = Success s }
arbitraryTestCase (Atomic combinator) = do
  ParserTestCase { parser, input, result } <- arbitraryTestCase combinator
  pure ParserTestCase { parser = Parser.Atomic parser, input, result = result }
arbitraryTestCase (LookAhead c) = do
  ParserTestCase { parser, input, result } <- arbitraryTestCase c
  pure ParserTestCase
    { parser = Parser.LookAhead parser
    , input
    , result
    }
arbitraryTestCase (Then (AnyCombinator c) c') = do
  ParserTestCase { parser, input = input1, result } <- arbitraryTestCase c
  ParserTestCase { parser = parser', input = input2, result = result' } <- arbitraryTestCase c'
  pure ParserTestCase
    { parser = Parser.Then parser parser'
    , input = input1 ++ input2
    , result = result'
    }
arbitraryTestCase (Before c (AnyCombinator c')) = do
  ParserTestCase { parser, input, result } <- arbitraryTestCase c
  ParserTestCase { parser = parser', input = input', result = result' } <- arbitraryTestCase c'
  pure ParserTestCase
    { parser = Parser.Before parser parser'
    , input = input ++ input'
    , result = result
    }
arbitraryTestCase (Fmap (AnyCombinator combinator)) = do
  f <- QC.arbitrary
  ParserTestCase { parser, input, result } <- arbitraryTestCase combinator
  pure ParserTestCase { parser = Parser.Fmap f parser, input, result = f <$> result }
arbitraryTestCase (Some c) = do
  ParserTestCase { parser, input, result } <- arbitraryTestCase c
  n <- chooseInt (1, 100)
  pure ParserTestCase
    { parser = Parser.Some parser
    , input = concat $ replicate n input
    , result = Success $ replicate n $ extractSuccess result
    }
arbitraryTestCase (Many c) = do
  ParserTestCase { parser, input, result } <- arbitraryTestCase c
  n <- QC.oneof [pure 0, chooseInt (1, 100)]
  pure ParserTestCase
    { parser = Parser.Many parser
    , input = concat $ replicate n input
    , result = Success $ replicate n $ extractSuccess result
    }
arbitraryTestCase (Alternative c c') = do
  ParserTestCase { parser, input, result } <- arbitraryTestCase c
  ParserTestCase { parser = parser', input = input', result = result' } <- arbitraryTestCase c'
  pure ParserTestCase
    { parser = Parser.Alternative parser parser'
    , input = input -- since the LHS succeeds, we don't consume any input for the right one
    , result = result -- since the LHS succeeds
    }

extractSuccess :: Result String a -> a
extractSuccess (Success a) = a
extractSuccess (Failure _) = error "Expected success"
