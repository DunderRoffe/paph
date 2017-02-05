{-# LANGUAGE DeriveGeneric #-}

module Message where

import Data.Aeson
import GHC.Generics

data Message = Ready
             | Available  { ids :: [Int] }
             | Connect    { id :: Int } 
             | Disconnect
             | Get        { id :: Int } 
             | Update     { contents :: [Data] }
             | Delete     { content :: Data } 
             | Create     { id :: Int }
  deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message
