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
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Maybe
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
    (parser, inputConstraints) <- evalStateT (arbitraryParserWithInputConstraints combinator) initGenParserInputsState
    testCases <- parserResult parser inputConstraints
    pure $ ParserTestCase parser testCases

testCase :: Gen (ParserTestCase String)
testCase = do
  let combinator = Then (AnyCombinator Str) Pure
  (parser, inputConstraints) <- evalStateT (arbitraryParserWithInputConstraints combinator) initGenParserInputsState
  testCases <- parserResult parser inputConstraints
  pure $ ParserTestCase parser testCases

-- Resolve the constraints and generate specific test cases with expected results
-- parserResult :: Parser.Parser a -> [CharConstraint] -> Gen TestCases
parserResult :: Parser.Parser a -> [CharConstraint] -> Gen (TestCases a)
parserResult (Parser.Pure a) _ = pure [([], Success a)]
parserResult (Parser.Chr char) (OneOf [char']:cs) = do
  when (char /= char') $ error "Char parser does not match constraints"
  pure [([char], Success char)]
parserResult (Parser.Str str) cs = do
  when (str /= map (\(OneOf [ch]) -> ch) (take (length str) cs)) $ error "String parser does not match constraints"
  pure [(str, Success str)]
parserResult (Parser.Then p p') cs = do
  (input, _) <- fromJust <$> parserResultSafe p cs
  (input', result') <- fromJust <$> parserResultSafe p' (drop (length input) cs)
  pure [(input ++ input', result')]

parserResultSafe :: Parser.Parser a -> [CharConstraint] -> Gen (Maybe (String, Result String a))
parserResultSafe p cs = do
  result <- parserResult p cs
  pure $ case result of
    [(input, Success result)] -> Just (input, Success result)
    _                         -> Nothing


  -- something like this for the Item case
  -- do
  -- cases <- mapM (\constraint -> do
  --   case constraint of
  --     OneOf chars -> do
  --       c <- elements chars
  --       pure (c, Success c)
  --     AnyChar -> do
  --       c <- QC.arbitrary
  --       pure (c, Success c)
  --   ) inputConstraints
  -- pure ParserTestCase { parser, cases }

-- TODO: the list of char constraints should probably be a dequeue
-- Choose a *specific* parser and generate the input constraints for that parser
arbitraryParserWithInputConstraints :: (Arbitrary a, Show a) => Combinator a -> GenParserInputs (Parser.Parser a, [CharConstraint])
arbitraryParserWithInputConstraints Pure = do
  a <- lift QC.arbitrary
  pure (Parser.Pure a, [])
arbitraryParserWithInputConstraints Satisfy = undefined
arbitraryParserWithInputConstraints Chr = do
  c <- arbitraryConstrainedChar
  pure (Parser.Chr c, [OneOf [c]])
arbitraryParserWithInputConstraints Str = do
  n <- lift $ chooseInt (1, stringMaxSize)
  str <- replicate n <$> arbitraryConstrainedChar
  pure (Parser.Str str, map (OneOf . (:[])) str)
arbitraryParserWithInputConstraints (Atomic c) = undefined
arbitraryParserWithInputConstraints (LookAhead c) = undefined
arbitraryParserWithInputConstraints Item = undefined
arbitraryParserWithInputConstraints (Then (AnyCombinator c) c') = do
  (parser, inputConstraints) <- arbitraryParserWithInputConstraints c
  (parser', inputConstraints') <- arbitraryParserWithInputConstraints c'
  pure (Parser.Then parser parser', inputConstraints ++ inputConstraints')
arbitraryParserWithInputConstraints (Before c (AnyCombinator c')) = undefined
arbitraryParserWithInputConstraints (Fmap (AnyCombinator c)) = undefined
arbitraryParserWithInputConstraints (Some c) = undefined
arbitraryParserWithInputConstraints (Many c) = undefined
arbitraryParserWithInputConstraints (Alternative c c') = undefined

extractSuccess :: Result String a -> a
extractSuccess (Success a) = a
extractSuccess (Failure _) = error "Expected success"

consumeChar :: GenParserInputsState -> GenParserInputsState
consumeChar s@(GenParserInputsState { follows, precludes }) = s { follows = tailsOrEmpty follows, precludes = tailsOrEmpty precludes }

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

arbitraryConstrainedChar :: StateT GenParserInputsState Gen Char
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
