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
import           Data.Bifunctor
import           Parameters
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
    combinator <- evalGenCombinator Combinator.arbitrary
    (parser, constraints) <- evalStateT (arbitraryParserWithConstraints combinator) initGenParserInputsState
    testCases <- resolveConstraints parser constraints
    pure $ ParserTestCase parser testCases

testCase :: Gen (ParserTestCase Char)
testCase = do
  let combinator = Before (Then (AnyCombinator (Atomic (Fmap (AnyCombinator Item)) :: Combinator Char)) (Fmap (AnyCombinator Satisfy))) (AnyCombinator (Then (AnyCombinator Satisfy) Satisfy))
  (parser, constraints) <- evalStateT (arbitraryParserWithConstraints combinator) initGenParserInputsState
  testCases <- resolveConstraints parser constraints
  pure $ ParserTestCase parser testCases

-- Resolve the constraints and generate specific test cases with expected results
-- parserResult :: Parser.Parser a -> [CharConstraint] -> Gen TestCases
resolveConstraints :: Parser.Parser a -> [CharConstraint] -> Gen [TestCase a]
resolveConstraints (Parser.Pure a) _ = pure [([], Success a)]
resolveConstraints (Parser.Satisfy p) (OneOf cs:_) = pure $ map (\c -> ([c], Success c)) cs
resolveConstraints (Parser.Chr char) (OneOf [char']:cs) = do
  when (char /= char') $ error "Char parser does not match constraints"
  pure [([char], Success char)]
resolveConstraints (Parser.Str str) cs = do
  when (str /= map (\(OneOf [ch]) -> ch) (take (length str) cs)) $ error "String parser does not match constraints"
  pure [(str, Success str)]
resolveConstraints (Parser.Atomic p) cs = resolveConstraints p cs
resolveConstraints (Parser.Then p p') cs =
  resolveSequencingParserConstraints p p' cs snd
resolveConstraints Parser.Item (AnyChar:_) = do
  cs <- resize inputsPerAnyCharConstraint $ listOf1 QC.arbitrary
  pure [([c], Success c) | c <- cs]
resolveConstraints (Parser.Before p p') cs =
  resolveSequencingParserConstraints p p' cs fst
resolveConstraints (Parser.Fmap f p) cs = do
  cases <- resolveConstraints p cs
  pure $ map (second (f <$>)) cases
resolveConstraints (Parser.Some p) cs = do
  cases <- resolveConstraints p cs
  pure $ [(input, (:[]) <$> result) | (input, result) <- cases]
resolveConstraints _ _ = error "Invalid case in constraint resolver" undefined

-- TODO: the list of char constraints should probably be a dequeue
-- Choose a *specific* parser and generate the input constraints for that parser
arbitraryParserWithConstraints :: (Arbitrary a, Show a) => Combinator a -> GenParserInputs (Parser.Parser a, [CharConstraint])
arbitraryParserWithConstraints Pure = do
  a <- lift $ resize literalSize QC.arbitrary
  pure (Parser.Pure a, [])
arbitraryParserWithConstraints Satisfy = do
  modify consumeChar
  n <- lift $ chooseInt (1, charsPerSatisfyPredicate)
  cs <- lift $ replicate n <$> QC.arbitrary
  pure (Parser.Satisfy (`elem` cs), [OneOf cs])
arbitraryParserWithConstraints Chr = do
  c <- arbitraryConstrainedChar
  pure (Parser.Chr c, [OneOf [c]])
arbitraryParserWithConstraints Str = do
  n <- lift $ chooseInt (1, literalSize)
  str <- replicate n <$> arbitraryConstrainedChar
  pure (Parser.Str str, map (OneOf . (:[])) str)
arbitraryParserWithConstraints (Atomic c) = do
  (parser, constraints) <- arbitraryParserWithConstraints c
  pure (Parser.Atomic parser, constraints)
arbitraryParserWithConstraints (LookAhead c) = undefined
arbitraryParserWithConstraints Item = do
  modify consumeChar
  pure (Parser.Item, [AnyChar])
arbitraryParserWithConstraints (Then (AnyCombinator c) c') = do
  (parser, constraints) <- arbitraryParserWithConstraints c
  (parser', constraints') <- arbitraryParserWithConstraints c'
  pure (Parser.Then parser parser', constraints ++ constraints')
arbitraryParserWithConstraints (Before c (AnyCombinator c')) = do
  (parser, constraints) <- arbitraryParserWithConstraints c
  (parser', constraints') <- arbitraryParserWithConstraints c'
  pure (Parser.Before parser parser', constraints ++ constraints')
arbitraryParserWithConstraints (Fmap (AnyCombinator c)) = do
  (parser, constraints) <- arbitraryParserWithConstraints c
  f <- lift QC.arbitrary
  pure (Parser.Fmap f parser, constraints)
arbitraryParserWithConstraints (Some c) = do
  (parser, constraints) <- arbitraryParserWithConstraints c
  -- Some will consume as much as possible so `some` must consume the follow and preclude sets if
  -- the sub-parser constraints are covered
  modify (\s@(GenParserInputsState{follows}) -> s { follows = dropWhile (constraintCovered constraints) follows })
  -- TODO: If the follow-set is empty, we may randomly choose to generate more input to be consumed by
  -- the sub-parser
  -- The constraints of the sub-parser are precluded. This is to ensure the some combinator does not
  -- consume the inputs intended for the next parser in the sequence
  modify (\s@(GenParserInputsState{precludes}) -> s { precludes = precludes ++ constraints })
  pure (Parser.Some parser, constraints)
arbitraryParserWithConstraints (Many c) = undefined

-- some(atomic(char 'a' *> lookAhead(char 'a'))) *> char 'a' *> char 'b'
-- t0: follow = ['a',['a', 'b'],'a','b']
-- t1: follow = ['b','a','b'] char eval
-- t1: follow = ['a','a','b'] lookAhead eval
-- t1: follow = ['a','b'] some eval again
-- t1: follow = ['a','b'] some no longer valid as ['a','a'] not in follow
-- t1: follow = ['b'] char eval
-- t1: follow = [] char eval

extractSuccess :: Result String a -> a
extractSuccess (Success a) = a
extractSuccess (Failure _) = error "Expected success"

consumeChar :: GenParserInputsState -> GenParserInputsState
consumeChar s@(GenParserInputsState { follows, precludes }) = s {
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
      -- if c is the final precluding constraint, we must not select from xs. Otherwise, we can
      , (if all (== AnyChar) constraints then 0 else 1, QC.arbitrary)
      ]
arbitraryPrecludedChar _ = QC.arbitrary

arbitraryConstrainedChar :: GenParserInputs Char
arbitraryConstrainedChar = do
  GenParserInputsState { follows, precludes } <- get
  modify consumeChar
  lift $ case follows of
    (OneOf cs:_) -> elements cs
    _            -> arbitraryPrecludedChar precludes

arbitraryCharExcluding :: [Char] -> Gen Char
arbitraryCharExcluding xs = elements [c | c <- [' ', '~'], c `notElem` xs]

evalGenParserInputs :: GenParserInputs t -> QC.Gen t
evalGenParserInputs gen = evalStateT gen initGenParserInputsState

resolveSequencingParserConstraints :: Parser.Parser a1 -> Parser.Parser a2 -> [CharConstraint] -> ((Result String a1, Result String a2) -> b) -> Gen [([Char], b)]
resolveSequencingParserConstraints p p' cs f = do
  cases <- resolveConstraints p cs
  let cs' = drop (testCasesInputLength cases) cs
  cases' <- resolveConstraints p' cs'
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
