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
import qualified Parser.Parser                  as Parser
import           Parser.ParserTestCase
import           Test.Tasty.QuickCheck          as QC hiding (Failure, Success)
import           Text.Gigaparsec                hiding (result)

type GenParserInputs t = StateT GenParserInputsState QC.Gen t

-- Constraints on the following n characters
data GenParserInputsState = GenParserInputsState {
  follows   :: [CharConstraint],
  precludes :: [CharConstraint]
}

initGenParserInputsState :: GenParserInputsState
initGenParserInputsState = GenParserInputsState
  { follows = [], precludes = [] }

instance (Arbitrary a, ArbitraryCombinator (Combinator a), Show a) => Arbitrary (ParserTestCase a) where
  arbitrary :: (Arbitrary a, ArbitraryCombinator (Combinator a), Show a) => Gen (ParserTestCase a)
  arbitrary = do
    combinator <- evalGenCombinatorState Combinator.arbitrary
    (parser, inputConstraints) <- evalStateT (arbitraryParserWithInputConstraints combinator) initGenParserInputsState
    pure $ parserResult parser inputConstraints


-- Resolve the constraints an generate specific test cases with expected results
parserResult :: Parser.Parser a -> [CharConstraint] -> ParserTestCase a
parserResult parser inputConstraints = undefined

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
-- inputs
-- "aaab bazb caztf"
-- invalid inputs
-- "aaab bazb cazbf" (invalid as many consumes all)

-- Let's say the bracketing is as follows to guide the recursive example
-- (many(satisfy isLower *> a *> item *> b) *> item) *> ((a *> z) *> (t *> item))
-- *>: recurse left and find the follow set and inputs
-- lhs: [a*b]

-- so basically we can always pick randomly unless it is the last non-any character is in the
-- precludes set

-- question: do i need multiple precludes sets?
-- no but we need to support predicates so each entry can be a set of allowed characters

-- lookAhead(char *> char) *> many(char)
-- lookAhead(a *> b) *> many(a)
-- this is fine as long as lookAhead doesn't add to inputs it only add to the follow set
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

-- lookAhead(some(satisfy (`elem` [x1, x2]))) *> some(y) *> char z
-- follow [[x1, x2], [x1, x2]] then y is chosen s.t. y == x1 (or x2 but let's use x1 for example)
-- now, follow [[x1, x2]] and precludes [[x1]] so z = x2

-- lookAhead(some(satisfy (`elem` [x1]))) *> some(y) *> char z
-- follow [[x1], [x1]] then y is chosen s.t. y == x1
-- some will consume the follow-set so follow = [], precludes = [x1]
-- now z chosen such that z != x1

-- lookahead(satisfy (`elem` [x1, x2])) *> lookahead(some(satisfy (`elem` ys@[y1, y2]))) *> char
-- follow [[x1, x2]] then ys is chosen as a random subset of [x1, x2], follow = []
-- repetitions is chosen as 2 so inputs = [[y1, y2], [y1, y2]]
-- precludes = inputs = [[y1, y2], [y1, y2]]
-- then we evaluate the lookAhead so
-- follows = inputs = [[y1, y2], [y1, y2]]
-- precludes = []
-- inputs = []
-- z is chosen s.t. z `elem` [y1, y2]
-- inputs = [z]

-- TODO: the list of char constraints should probably be a dequeue
-- Choose a *specific* parser and generate the input constraints for that parser
arbitraryParserWithInputConstraints :: (Arbitrary a, Show a) => Combinator a -> GenParserInputs (Parser.Parser a, [CharConstraint])
arbitraryParserWithInputConstraints Pure = do
  a <- lift QC.arbitrary
  pure (Parser.Pure a, [OneOf []])
-- arbitraryParserWithInputConstraints Satisfy = do
--   state <- get
--   chars <- undefined
--   c <- lift $ QC.oneof $ map pure chars
--   pure ParserTestCase { parser = Parser.Satisfy (`elem` chars), inputs = [c], result = Success c }
arbitraryParserWithInputConstraints Chr = do
  GenParserInputsState { follows, precludes } <- get
  modify consumeChar
  c <- lift $ case follows of
    (OneOf cs:_) -> QC.elements cs
    _            -> arbitraryPrecluding precludes
  pure (Parser.Chr c, [OneOf [c]])
arbitraryParserWithInputConstraints Item = pure (Parser.Item, [])
-- arbitraryParserWithInputConstraints (Then (AnyCombinator c) c') = do
--   ParserTestCase { parser, inputs, result } <- arbitraryParserWithInputConstraints c
--   ParserTestCase { parser = parser', inputs = inputs', result = result' } <- arbitraryParserWithInputConstraints c'
--   pure ParserTestCase
--     { parser = Parser.Then parser parser'
--     , inputs = inputs ++ inputs'
--     , result = result'
--     }
-- arbitraryParserWithInputConstraints (Before c (AnyCombinator c')) = do
--   ParserTestCase { parser, inputs, result } <- arbitraryParserWithInputConstraints c
--   ParserTestCase { parser = parser', inputs = inputs', result = result' } <- arbitraryParserWithInputConstraints c'
--   pure ParserTestCase
--     { parser = Parser.Before parser parser'
--     , inputs = inputs ++ inputs'
--     , result = result
--     }

extractSuccess :: Result String a -> a
extractSuccess (Success a) = a
extractSuccess (Failure _) = error "Expected success"

consumeChar :: GenParserInputsState -> GenParserInputsState
consumeChar s@(GenParserInputsState { follows, precludes }) = s { follows = tailsOrEmpty follows, precludes = tailsOrEmpty precludes }

tailsOrEmpty :: [a] -> [a]
tailsOrEmpty []     = []
tailsOrEmpty (_:xs) = xs

-- assertFollowsNonOverlapping :: Applicative f => GenParserInputsState -> f ()
-- assertFollowsNonOverlapping (GenParserInputsState { follows, precludes }) =
  -- when (all (\fs -> all (all (`notElem` fs)) precludes) follows) $ error "Overlapping follows and non-overlapping"

-- arbitraryCharsWithFollowConstraints :: GenParserInputsState -> Gen Char
-- arbitraryCharsWithFollowConstraints (GenParserInputsState { follows = (OneOf xs):constraints }) =
--   chooseAny xs

arbitraryPrecluding :: [CharConstraint] -> Gen Char
arbitraryPrecluding (c@(OneOf xs):constraints) = do
  frequency
      [ (1, arbitraryCharExcluding xs)
      -- if c is the final precluding constraint, we must not select from xs. Otherwise, we can
      , (if all (== AnyChar) constraints then 0 else 1, QC.arbitrary)
      ]
arbitraryPrecluding _ = QC.arbitrary

arbitraryCharExcluding :: [Char] -> Gen Char
arbitraryCharExcluding xs = elements [c | c <- [' ', '~'], c `notElem` xs]
