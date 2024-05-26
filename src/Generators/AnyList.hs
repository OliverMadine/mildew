{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Generators.AnyList where

import qualified Test.Tasty.QuickCheck as QC

data AnyList = forall a. (QC.CoArbitrary a, Show a) => AnyList [a]
deriving instance Show AnyList

instance QC.Arbitrary AnyList where
  arbitrary :: QC.Gen AnyList
  arbitrary = QC.oneof
    [ AnyList <$> (QC.arbitrary :: QC.Gen [Char])
    , AnyList <$> (QC.arbitrary :: QC.Gen [String])
    , AnyList <$> (QC.arbitrary :: QC.Gen [Int])
    , AnyList <$> (QC.arbitrary :: QC.Gen [Bool])
    ]

instance QC.CoArbitrary AnyList where
  coarbitrary :: AnyList -> QC.Gen b -> QC.Gen b
  coarbitrary (AnyList xs) = QC.coarbitrary xs
