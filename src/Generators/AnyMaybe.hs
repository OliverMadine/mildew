{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Generators.AnyMaybe where

import qualified Test.Tasty.QuickCheck as QC

data AnyMaybe = forall a. (QC.CoArbitrary a, Show a) => AnyMaybe (Maybe a)
deriving instance Show AnyMaybe

instance QC.Arbitrary AnyMaybe where
  arbitrary :: QC.Gen AnyMaybe
  arbitrary = QC.oneof
    [ AnyMaybe <$> (QC.arbitrary :: QC.Gen (Maybe Char))
    , AnyMaybe <$> (QC.arbitrary :: QC.Gen (Maybe String))
    , AnyMaybe <$> (QC.arbitrary :: QC.Gen (Maybe Int))
    , AnyMaybe <$> (QC.arbitrary :: QC.Gen (Maybe Bool))
    ]

instance QC.CoArbitrary AnyMaybe where
  coarbitrary :: AnyMaybe -> QC.Gen b -> QC.Gen b
  coarbitrary (AnyMaybe x) = QC.coarbitrary x
