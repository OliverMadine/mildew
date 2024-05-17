module GigaparsecCompiler where

compile :: Combinator a -> Parser as
compile Pure = pure ()