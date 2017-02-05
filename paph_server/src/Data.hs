{-# LANGUAGE DeriveGeneric #-}

module Data where

import Data.Aeson
import GHC.Generics

data Data = SLP | RP | INFO | ARK
  deriving (Show, Generic, Eq)

instance FromJSON Data
instance ToJSON Data
