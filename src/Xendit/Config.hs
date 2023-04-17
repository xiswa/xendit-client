{-# LANGUAGE DeriveGeneric #-}
module Xendit.Config (
    XenditConfig(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Servant.Client (BaseUrl)
import GHC.Generics
import Xiswa.Utils

data XenditConfig = XenditConfig
  { xenditApiKey        :: !Text
  , xenditApiUrl        :: !BaseUrl
  , xenditCallbackToken :: !Text
  }
  deriving (Eq, Show, Generic)

options :: Options
options = defaultOptions
  { fieldLabelModifier = camelToSnakeCase "xendit"
  }

instance ToJSON XenditConfig where
  toJSON = genericToJSON options

instance FromJSON XenditConfig where
  parseJSON = genericParseJSON options
