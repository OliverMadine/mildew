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
import           Control.Monad.Trans.State
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
    pure $ parserResult parser inputConstraints

arbitraryTestCase :: Gen (ParserTestCase String)
arbitraryTestCase = do
  let combinator = Then (AnyCombinator Str) Pure
  (parser, inputConstraints) <- evalStateT (arbitraryParserWithInputConstraints combinator) initGenParserInputsState
  pure $ parserResult parser inputConstraints

-- Resolve the constraints an generate specific test cases with expected results
parserResult :: Parser.Parser a -> [CharConstraint] -> ParserTestCase a
parserResult parser inputConstraints = undefined

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
arbitraryParserWithInputConstraints (Then (AnyCombinator c) c') = undefined
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
