module Main where

import Test.Tasty.QuickCheck (Arbitrary(arbitrary), Gen, generate)
import Combinators ( Combinator )

main :: IO ()
main = do
    parser <- generate (arbitrary :: Gen (Combinator Int))
    print parser
