module Utils.Print where

parensShow :: Show a => a -> String
parensShow s = '(' : show s ++ ")"
