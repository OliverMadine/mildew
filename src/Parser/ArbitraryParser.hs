{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE RecordWildCards         #-}

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
import           Parser.Parser                  (CharConstraint (..), Parser,
                                                 inputConstraints)
import qualified Parser.Parser                  as Parser
import           Parser.ParserTestCase
import           Test.Tasty.QuickCheck          as QC hiding (Failure, Success)
import           Text.Gigaparsec                hiding (result)

initGenParserState :: GenParserState
initGenParserState = GenParserState
  { follows = [], precludes = [] }

instance (Arbitrary a, Show a) => Arbitrary (ParserTestCases a) where
  arbitrary :: (Arbitrary a, Show a) => Gen (ParserTestCases a)
  arbitrary = do
    -- Generate a combinator structure
    combinator <- evalGenCombinator Combinator.arbitrary
    -- Instantiate an arbitrary parser for that structure
    (parser, state) <- runStateT (arbitraryParser combinator) initGenParserState
    -- Consider any remaining constraints on the input to the parser
    remainingInput <- evalStateT consumeRemainingConstraints state
    -- Generate specific test cases consisting of input string wit their corresponding expected results
    let testCases = [(input ++ remainingInput, result) | (input, result) <- modelResults parser]
    pure $ ParserTestCases parser testCases

testCase :: Gen (ParserTestCases Char)
testCase = do
  let combinator = Then (AnyCombinator (LookAhead (Then (AnyCombinator Chr) Chr))) Satisfy
  (parser, state) <- runStateT (arbitraryParser combinator) initGenParserState
  let testCases = modelResults parser
  remainingInput <- evalStateT consumeRemainingConstraints state
  let testCases' = [(input ++ remainingInput, result) | (input, result) <- testCases]
  pure $ ParserTestCases parser testCases'

-- Resolve the constraints and generate specific test cases with expected results
modelResults :: Parser a -> [TestCase a]
modelResults (Parser.Pure a) = [([], Success a)]
modelResults (Parser.Satisfy cs p) = map (\c -> ([c], Success c)) cs
modelResults (Parser.Chr char) = [([char], Success char)]
modelResults (Parser.Item cs) = [([c], Success c) | c <- cs]
modelResults (Parser.Str str) = [(str, Success str)]
modelResults (Parser.Atomic p) = modelResults p
modelResults (Parser.LookAhead p) = let cases = modelResults p in map (\(i, r) -> ("", r)) cases
modelResults (Parser.Then p p') = modelSequencingParser p p' snd
modelResults (Parser.Before p p') = modelSequencingParser p p' fst
modelResults (Parser.Fmap f p) = map (second (f <$>)) (modelResults p)
modelResults (Parser.Some n p) =
  let cases = replicateM n (modelResults p) in
    map (foldl1 combineRepeatedTestCase . map (second (fmap (: [])))) cases
modelResults (Parser.Many n p) = undefined

-- TODO: the list of char constraints should be a dequeue for performance reasons
-- Choose a specific parser while tracking specific input constraints on the Parser GADT
arbitraryParser :: (Arbitrary a, Show a) => Combinator a -> GenParser (Parser a)
arbitraryParser Pure = do
  a <- lift $ resize literalSize QC.arbitrary
  pure $ Parser.Pure a
arbitraryParser Satisfy = do
  -- This makes the remaining input problem easier as we know which character was chosen when
  -- calculating the expected result for the test case (e.g. when fmap is applied, we need to know this)
  -- n <- lift $ chooseInt (1, charsPerSatisfyPredicate)
  n <- lift $ chooseInt (1, charsPerSatisfyPredicate)
  cs <- replicateM n arbitraryConstrainedChar
  pure $ Parser.Satisfy cs (`elem` cs)
arbitraryParser Chr = Parser.Chr <$> arbitraryConstrainedChar
arbitraryParser Item = do
  modify consumeChar
  pure $ Parser.Item undefined
arbitraryParser Str = do
  GenParserState{follows} <- get
  n <- lift $ chooseInt (1, literalSize)
  str <- replicateM (max (length follows) n) arbitraryConstrainedChar
  pure $ Parser.Str str
arbitraryParser (Atomic c) = do
  parser <- arbitraryParser c
  pure $ Parser.Atomic parser
arbitraryParser (LookAhead c) = do
  parser <- arbitraryParser c
  modify (\s@(GenParserState{follows}) -> s { follows = inputConstraints parser ++ follows })
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
arbitraryParser (Some c) = do
  parser <- arbitraryParser c
  let newPrecludingConstraint = take 1 $ inputConstraints parser
      (consumedCount, follows, precludes) = consumeConstraints parser follows precludes
  modify (\s@(GenParserState{precludes}) -> s {
      follows,
      precludes = newPrecludingConstraint ++ precludes
    })
  if null follows
    then do
      -- Chose random number of additional repetitions
      n <- lift $ chooseInt (0, additionalRepetitions)
      return (Parser.Some (succ consumedCount + n) parser)
    else do
      return (Parser.Some (succ consumedCount) parser)
arbitraryParser (Many c) = undefined

consumeConstraints :: Parser a1 -> [CharConstraint] -> t0 -> (Int, [CharConstraint], t0)
consumeConstraints = undefined

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

modelSequencingParser :: Parser a1 -> Parser a2 -> ((Result String a1, Result String a2) -> b) -> [([Char], b)]
modelSequencingParser p p' f = 
  let cases = modelResults p
      cases' = modelResults p'
    in zipWith (\(i, r) (i', r') -> (i ++ i', f (r, r'))) cases cases'

testCasesInputLength :: [TestCase a] -> Int
testCasesInputLength [] = error "Cannot find input length for empty test cases"
testCasesInputLength ((input, _):testCases) =
  assert (all (\(i, _) -> length i == length input) testCases) (length input)

-- If no matter what we choose in the follow set, it parses, then the constraint is covered
constraintCovered :: [CharConstraint] -> [CharConstraint] -> Bool
constraintCovered (OneOf cs:constraints) (OneOf cs':constraints') =
  all (`elem` cs) cs' && constraintCovered constraints constraints'
constraintCovered (AnyChar:_) _ = True
constraintCovered _ (AnyChar:_) = False
constraintCovered [] _ = True
constraintCovered _ [] = False
