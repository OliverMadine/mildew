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
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Maybe                     (fromMaybe)
import qualified Parser.Parser                  as Parser
import           Parser.ParserTestCase
import           Test.Tasty.QuickCheck          as QC hiding (Failure, Success)
import           Text.Gigaparsec                hiding (result)

type GenParserTestCase t = StateT GenParserTestCaseState QC.Gen t

data CharConstraint = OneOf [Char] | AnyChar

-- Constraints on the following n chars
data GenParserTestCaseState = GenParserTestCaseState {
  -- Must follow
  follows   :: [CharConstraint],
  -- Must not all follow
  precludes :: [CharConstraint]
}

initGenParserTestCaseState :: GenParserTestCaseState
initGenParserTestCaseState = GenParserTestCaseState
  { follows = [], precludes = [] }

instance (Arbitrary a, ArbitraryCombinator (Combinator a), Show a) => Arbitrary (ParserTestCase a) where
  arbitrary :: (Arbitrary a, ArbitraryCombinator (Combinator a), Show a) => Gen (ParserTestCase a)
  arbitrary = do
    combinator <- evalGenCombinatorState Combinator.arbitrary
    evalStateT (arbitraryTestCase combinator) initGenParserTestCaseState

-- many(satisfy f *> char *> char *> string) *> item *> char *> char *> char *> char
-- make the above work
-- think about failing atomics
-- think about lookahead
-- think about <|> much later

-- many(satisfy f *> char *> item *> string) *> item *> char *> char *> char *> item
-- parser
-- many(satisfy isLower *> a *> item *> b) *> item *> a *> z *> t *> item
-- invalid parser
-- many(satisfy isLower *> a *> item *> b) *> item *> a *> z *> b *> item
-- input
-- "aaab bazb caztf"
-- invalid input
-- "aaab bazb cazbf" (invalid as many consumes all)

-- Let's say the bracketing is as follows to guide the recursive example
-- (many(satisfy isLower *> a *> item *> b) *> item) *> ((a *> z) *> (t *> item))
-- *>: recurse left and find the follow set and input
-- lhs: [a*b]

-- so basically we can always pick randomly unless it is the last non-any character in the precludes
-- set

-- question: do i need multiple precludes sets?
-- no but we need to support predicates so each entry can be a set of allowed characters

-- lookAhead(char *> char) *> many(char)
-- lookAhead(a *> b) *> many(a)
-- this is fine as long as lookAhead doesn't add to input it only add to the follow set
-- then we take parse (lookAhead(a *> b) *> many(a)) "ab"

-- ok this isn't too bad, ready

-- many(c1 *> item) *> lookAhead (c2) *> char c3
-- how do we handle this? What if c1 == c2? Can we avoid that
-- right now we would say precludes([c1, *]) then we would consume lookAhead (c2) which would see the precludes [c1 *]
-- so it would be forced to choose something else
-- so we're all good
-- I should run through this example in the paper

-- lookAhead (c2) *> many(c1 *> item) *> char c3
-- follow c2 then either
-- 1. choose c1 s.t. c1 == c2, many replicates at least once then follows is [] and precludes [c1] so c1 != c3
-- 2. choose c1 s.t. c1 != c2 many replicates zero times so follows is still [c1] and precludes is []

-- lookAhead(satisfy (`elem` xs) *> z) *> some(x) *> y
-- follow [xs, z] then x is chosen such that x `elem` xs. follow = [z], precludes = [x]
-- if x == z then some will consume twice so follow = [], precludes = [x]
-- so for the implementation, we must consume the follow set for some/many
-- y is now chosen s.t. y != x


arbitraryTestCase :: (Arbitrary a, Show a) => Combinator a -> GenParserTestCase (ParserTestCase a)
arbitraryTestCase Pure = do
  a <- lift QC.arbitrary
  pure ParserTestCase { parser = Parser.Pure a, input = "", result = Success a }
arbitraryTestCase Satisfy = do
  state <- get
  chars <- undefined -- fromMaybe (lift . listOf1 $ arbitraryWithPrecludingConstraints state) (nextFollowingChars state)
  modify consumeFollowCharacter
  -- TODO: update the precludes set?
  c <- lift $ QC.oneof $ map pure chars
  pure ParserTestCase { parser = Parser.Satisfy (`elem` chars), input = [c], result = Success c }
arbitraryTestCase Chr = do
  c <- lift QC.arbitrary
  pure ParserTestCase { parser = Parser.Chr c, input = [c], result = Success c }
arbitraryTestCase Item = do
  c <- lift QC.arbitrary
  pure ParserTestCase { parser = Parser.Item, input = [c], result = Success c }
arbitraryTestCase Str = do
  s <- lift $ listOf1 QC.arbitrary
  pure ParserTestCase { parser = Parser.Str s, input = s, result = Success s }
arbitraryTestCase (Atomic combinator) = do
  ParserTestCase { parser, input, result } <- arbitraryTestCase combinator
  pure ParserTestCase { parser = Parser.Atomic parser, input, result = result }
arbitraryTestCase (LookAhead c) = do
  -- we can write this by having the inner parser consume the follow set then
  -- we can take the input produces add it to the new follow set and discard
  ParserTestCase { parser, input, result } <- arbitraryTestCase c
  pure ParserTestCase
    { parser = Parser.LookAhead parser
    , input
    , result
    }
arbitraryTestCase (Then (AnyCombinator c) c') = do
  ParserTestCase { parser, input, result } <- arbitraryTestCase c
  ParserTestCase { parser = parser', input = input', result = result' } <- arbitraryTestCase c'
  pure ParserTestCase
    { parser = Parser.Then parser parser'
    , input = input ++ input'
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
  f <- lift QC.arbitrary
  ParserTestCase { parser, input, result } <- arbitraryTestCase combinator
  pure ParserTestCase { parser = Parser.Fmap f parser, input, result = f <$> result }
arbitraryTestCase (Some c) = do
  ParserTestCase { parser, input, result } <- arbitraryTestCase c
  n <- lift $ chooseInt (1, 100)
  pure ParserTestCase
    { parser = Parser.Some parser
    , input = concat $ replicate n input
    , result = Success $ replicate n $ extractSuccess result
    }
arbitraryTestCase (Many c) = do
  ParserTestCase { parser, input, result } <- arbitraryTestCase c
  n <- lift $ QC.oneof [pure 0, chooseInt (1, 100)]
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

consumeFollowCharacter :: GenParserTestCaseState -> GenParserTestCaseState
consumeFollowCharacter s@(GenParserTestCaseState { follows = [] }) = s
consumeFollowCharacter s@(GenParserTestCaseState { follows = _:xs }) = s { follows = xs }

-- assertFollowsNonOverlapping :: Applicative f => GenParserTestCaseState -> f ()
-- assertFollowsNonOverlapping (GenParserTestCaseState { follows, precludes }) =
  -- when (all (\fs -> all (all (`notElem` fs)) precludes) follows) $ error "Overlapping follows and non-overlapping"

-- arbitraryCharsWithFollowConstraints :: GenParserTestCaseState -> Gen Char
-- arbitraryCharsWithFollowConstraints (GenParserTestCaseState { follows = (OneOf xs):constraints }) = 
--   chooseAny xs 

-- arbitraryWithPrecludingConstraints :: GenParserTestCaseState -> Gen Char
-- arbitraryWithPrecludingConstraints (GenParserTestCaseState { precludes = c@(OneOf xs):constraints }) = do
--   frequency
--       [ (1, arbitraryCharExcluding xs)
--       -- if c is the final precluding constraint, we must not select from xs. Otherwise, we can 
--       , (if all (== AnyChar) constraints then 0 else 1, QC.arbitrary)
--       ]
-- arbitraryWithPrecludingConstraints _ = QC.arbitrary

arbitraryCharExcluding :: [Char] -> Gen Char
arbitraryCharExcluding xs = elements [c | c <- [' ', '~'], c `notElem` xs]
