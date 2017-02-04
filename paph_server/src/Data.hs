{-# LANGUAGE DeriveGeneric #-}

module Data where

import Data.Aeson

data Data = SLP | RP | INFO | ARK
  deriving (Show, Generic)

instance FromJSON Data
instance ToJSON Data
