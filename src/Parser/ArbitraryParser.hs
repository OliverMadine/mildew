{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE UndecidableInstances    #-}

module Parser.ArbitraryParser where

import           Combinator.ArbitraryCombinator as Combinator
import           Combinator.Combinator
import           Combinator.GenCombinator
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Debug.Trace
import           Parameters
import           Parser.Parser                  (Parser)
import qualified Parser.Parser                  as Parser
import           Parser.ParserTestCase
import           Test.Tasty.QuickCheck          as QC hiding (Failure, Success)
import           Text.Gigaparsec                hiding (result)

initGenParserState :: GenParserState
initGenParserState = GenParserState
  { follows = [], precludes = [] }

instance (Arbitrary a, ArbitraryCombinator (Combinator a), Show a) => Arbitrary (ParserTestCase a) where
  arbitrary :: (Arbitrary a, ArbitraryCombinator (Combinator a), Show a) => Gen (ParserTestCase a)
  arbitrary = do
    combinator <- evalGenCombinator Combinator.arbitrary
    (parser, state) <- runStateT (arbitraryParser combinator) initGenParserState
    remainingInput <- evalStateT consumeRemainingConstraints state
    testCases <- arbitraryTestCases parser
    let testCases' = traceShow state $ [(input ++ remainingInput, result) | (input, result) <- testCases]
    pure $ ParserTestCase parser testCases'

testCase :: Gen (ParserTestCase Char)
testCase = do
  let combinator = Then (AnyCombinator (LookAhead Chr)) (Before (Then (AnyCombinator Chr) Pure) (AnyCombinator Chr))
  parser <- evalStateT (arbitraryParser combinator) initGenParserState
  testCases <- arbitraryTestCases parser
  pure $ ParserTestCase parser testCases

-- Resolve the constraints and generate specific test cases with expected results
arbitraryTestCases :: Parser a -> Gen [TestCase a]
arbitraryTestCases (Parser.Pure a) = pure [([], Success a)]
arbitraryTestCases (Parser.Satisfy cs p) = pure $ map (\c -> ([c], Success c)) cs
arbitraryTestCases (Parser.Chr char) = pure [([char], Success char)]
arbitraryTestCases Parser.Item = do
  cs <- resize inputsPerAnyCharConstraint $ listOf1 QC.arbitrary
  pure [([c], Success c) | c <- cs]
arbitraryTestCases (Parser.Str str) = pure [(str, Success str)]
arbitraryTestCases (Parser.Atomic p) = arbitraryTestCases p
arbitraryTestCases (Parser.LookAhead p) = do
  cases <- arbitraryTestCases p
  pure $ map (\(i, r) -> ("", r)) cases
arbitraryTestCases (Parser.Then p p') = arbitrarySequencingParser p p' snd
arbitraryTestCases (Parser.Before p p') = arbitrarySequencingParser p p' fst
arbitraryTestCases (Parser.Fmap f p) = do
  cases <- arbitraryTestCases p
  pure $ map (second (f <$>)) cases
arbitraryTestCases (Parser.Some n p) = do
  cases <- replicateM n (arbitraryTestCases p)
  pure $ map (foldl1 combineRepeatedTestCase . map (second (fmap (: [])))) cases

-- TODO: the list of char constraints should be a dequeue for performance reasons
-- Choose a *specific* parser and generate the input constraints for that parser
arbitraryParser :: (Arbitrary a, Show a) => Combinator a -> GenParser (Parser a)
arbitraryParser Pure = do
  a <- lift $ resize literalSize QC.arbitrary
  pure $ Parser.Pure a
arbitraryParser Satisfy = do
  n <- lift $ chooseInt (1, charsPerSatisfyPredicate)
  cs <- replicateM n arbitraryConstrainedChar
  modify consumeChar
  pure $ Parser.Satisfy cs (`elem` cs)
arbitraryParser Chr = Parser.Chr <$> arbitraryConstrainedChar
arbitraryParser Item = do
  modify consumeChar
  pure Parser.Item
arbitraryParser Str = do
  n <- lift $ chooseInt (1, literalSize)
  str <- replicate n <$> arbitraryConstrainedChar
  pure $ Parser.Str str
arbitraryParser (Atomic c) = do
  parser <- arbitraryParser c
  pure $ Parser.Atomic parser
arbitraryParser (LookAhead c) = do
  GenParserState{follows} <- get
  parser <- arbitraryParser c
  modify (\s@(GenParserState{follows}) -> s { follows = follows })
  pure $ Parser.LookAhead parser
arbitraryParser (Then (AnyCombinator c) c') = do
  parser <- arbitraryParser c
  parser' <- arbitraryParser c'
  pure $ Parser.Then parser parser'
arbitraryParser (Before c (AnyCombinator c')) = do
  parser <- arbitraryParser c
  parser' <- arbitraryParser c'
  pure $ Parser.Before parser parser'
arbitraryParser (Fmap (AnyCombinator c)) = do
  parser <- arbitraryParser c
  f <- lift QC.arbitrary
  pure $ Parser.Fmap f parser
arbitraryParser (Some c) = undefined
  -- parser <- arbitraryParser c
  -- -- Some will consume as much as possible so `some` must consume the follow and preclude sets if
  -- -- the sub-parser constraints are covered
  -- modify (\s@(GenParserState{follows}) -> s { follows = dropWhile (constraintCovered constraints) follows })
  -- -- TODO: If the follow-set is empty, we may randomly choose to generate more input to be consumed by
  -- -- the sub-parser
  -- -- The constraints of the sub-parser are precluded. This is to ensure the some combinator does not
  -- -- consume the inputs intended for the next parser in the sequence
  -- modify (\s@(GenParserState{precludes}) -> s { precludes = precludes ++ constraints })
  -- pure Parser.Some 2 parser
arbitraryParser (Many c) = undefined

consumeRemainingConstraints :: GenParser String
consumeRemainingConstraints = do
  GenParserState{..} <- get
  if null follows
    then pure []
    else do
      c <- arbitraryConstrainedChar
      cs <- consumeRemainingConstraints
      pure $ c : cs

combineRepeatedTestCase :: TestCase [a] -> TestCase [a] -> TestCase [a]
combineRepeatedTestCase (i, Success r) (i', Success r') = (i ++ i', Success $ r ++ r')
combineRepeatedTestCase _ _ = error "Unexpected failure case in repeated parser"

extractSuccess :: Result String a -> a
extractSuccess (Success a) = a
extractSuccess (Failure _) = error "Expected success"

consumeChar :: GenParserState -> GenParserState
consumeChar s@(GenParserState { follows, precludes }) = s {
  follows = tailsOrEmpty follows,
  precludes = tailsOrEmpty precludes
  }

tailsOrEmpty :: [a] -> [a]
tailsOrEmpty []     = []
tailsOrEmpty (_:xs) = xs

arbitraryPrecludedChar :: [CharConstraint] -> Gen Char
arbitraryPrecludedChar (c@(OneOf xs):constraints) = do
  frequency
      [ (1, arbitraryCharExcluding xs)
      -- if c is the final precluding constraint, we must not select from xs. Otherwise, we can.
      , (if all (== AnyChar) constraints then 0 else 1, QC.arbitrary)
      ]
arbitraryPrecludedChar _ = QC.arbitrary

arbitraryConstrainedChar :: GenParser Char
arbitraryConstrainedChar = do
  GenParserState { follows, precludes } <- get
  modify consumeChar
  lift $ case follows of
    (OneOf cs:_) -> elements cs
    _            -> arbitraryPrecludedChar precludes

arbitraryCharExcluding :: [Char] -> Gen Char
arbitraryCharExcluding xs = elements [c | c <- [' ', '~'], c `notElem` xs]

evalGenParserInputs :: GenParser t -> QC.Gen t
evalGenParserInputs gen = evalStateT gen initGenParserState

arbitrarySequencingParser :: Parser a1 -> Parser a2 -> ((Result String a1, Result String a2) -> b) -> Gen [([Char], b)]
arbitrarySequencingParser p p' f = do
  cases <- arbitraryTestCases p
  cases' <- arbitraryTestCases p'
  pure $ zipWith (\(i, r) (i', r') -> (i ++ i', f (r, r'))) cases cases'

testCasesInputLength :: [TestCase a] -> Int
testCasesInputLength [] = error "Cannot find input length for empty test cases"
testCasesInputLength ((input, _):testCases) =
  assert (all (\(i, _) -> length i == length input) testCases) (length input)

-- Needs to be atomic in a sense
-- If no matter what we choose in the follow set, it parses, then the constraint is covered
constraintCovered :: [CharConstraint] -> CharConstraint -> Bool
constraintCovered (OneOf cs:_) (OneOf cs') = all (`elem` cs) cs'
constraintCovered (AnyChar:_) _ = True
constraintCovered [] _ = error "Cannot check for constraint coverage without constraints"
