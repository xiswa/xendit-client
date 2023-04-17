{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Xendit.Api.Common (
    Currency(..)
  , Status(..)
  , BankCode(..)
  , Bank(..)
  , EWallet(..)
  , RetailOutlet(..)
  , QRCode(..)
  , DirectDebit(..)
  , PayLater(..)
  , Customer(..)
  , Fee(..)
  , PaymentDetail(..)
  , Item(..)
  , getAuth
  , xenditOptions
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import Servant.API
import GHC.Generics
import Xiswa.Utils
import Xendit.Config

xenditOptions :: Options
xenditOptions = defaultOptions
  { omitNothingFields = True
  }

getAuth
  :: forall env m.
     ( MonadReader env m
     , Has XenditConfig env
     )
  => m BasicAuthData  
getAuth = do
  XenditConfig{..} <- grab
  return $ BasicAuthData (encodeUtf8 xenditApiKey) ""


data Currency = IDR | PHP
  deriving (Eq, Show, Generic)

instance ToJSON Currency where
  toJSON = genericToJSON xenditOptions

instance FromJSON Currency where
  parseJSON = genericParseJSON xenditOptions


data Status = PENDING | PAID | EXPIRED | SETTLED
  deriving (Eq, Show, Generic)

instance ToJSON Status where
  toJSON = genericToJSON xenditOptions

instance FromJSON Status where
  parseJSON = genericParseJSON xenditOptions


data BankCode = 
    BCA 
  | BNI 
  | BRI 
  | BJB 
  | BSI 
  | CIMB 
  | DBS 
  | MANDIRI 
  | PERMATA 
  | BNC 
  | SAHABAT_SAMPOERNA
  deriving (Eq, Show, Generic)

instance ToJSON BankCode where
  toJSON = genericToJSON xenditOptions

instance FromJSON BankCode where
  parseJSON = genericParseJSON xenditOptions


-- | A simplified Xendit bank object
-- Most fields are encoded as Text for simplicity.
data Bank = Bank
  { bankCode              :: !BankCode
  , bankCollectionType    :: !Text
  , bankTransferAmount    :: !Int
  , bankBranch            :: !Text
  , bankAccountHolderName :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Bank where
  toJSON (Bank code typ amount branch holderName) =
    object [ "bank_code"            .= code
           , "collection_type"      .= typ
           , "transfer_amount"      .= amount
           , "bank_branch"          .= branch
           , "account_holder_name"  .= holderName
           ]

instance FromJSON Bank where
  parseJSON = withObject "Bank" $ \obj ->
    Bank <$> obj .: "bank_code"
         <*> obj .: "collection_type"
         <*> obj .: "transfer_amount"
         <*> obj .: "bank_branch"
         <*> obj .: "account_holder_name"


data EWallet =
    DANA
  | OVO
  | LINKAJA
  | SHOPEEPAY
  | GCASH
  | GRABPAY
  | PAYMAYA
  deriving (Eq, Show, Generic)

instance ToJSON EWallet where
  toJSON = genericToJSON xenditOptions

instance FromJSON EWallet where
  parseJSON = genericParseJSON xenditOptions


newtype RetailOutlet = RetailOutlet
  { retailOutletName    :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON RetailOutlet where
  toJSON = genericToJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase ""
      }

instance FromJSON RetailOutlet where
  parseJSON = genericParseJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase ""
      }


newtype QRCode = QRCode
  { qrCodeType          :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON QRCode where
  toJSON = genericToJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase ""
      }

instance FromJSON QRCode where
  parseJSON = genericParseJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase ""
      }


newtype DirectDebit = DirectDebit
  { directDebitType     :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON DirectDebit where
  toJSON = genericToJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase ""
      }

instance FromJSON DirectDebit where
  parseJSON = genericParseJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase ""
      }


newtype PayLater = PayLater
  { paylaterType        :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON PayLater where
  toJSON = genericToJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase ""
      }

instance FromJSON PayLater where
  parseJSON = genericParseJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase ""
      }


data Customer = Customer
  { customerGivenNames    :: Maybe Text
  , customerSurname       :: Maybe Text
  , customerEmail         :: Maybe Text
  , customerMobileNumber  :: Maybe Text
  , customerAddresses     :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Customer where
  toJSON = genericToJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase "customer"
      }

instance FromJSON Customer where
  parseJSON = genericParseJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase "customer"
      }


data Fee = Fee
  { typ     :: Text
  , value   :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON Fee where
  toJSON (Fee ty val) =
    object [ "type"  .= ty
           , "value" .= val
           ]

instance FromJSON Fee where
  parseJSON = withObject "Fee" $ \obj ->
    Fee <$> obj .: "type"
        <*> obj .: "value"


-- | Object containing payment details
-- Currently supporting QRIS only
data PaymentDetail = PaymentDetail
  { pdReceiptId   :: !Text
  , pdSource      :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON PaymentDetail where
  toJSON = genericToJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase "pd"
      }

instance FromJSON PaymentDetail where
  parseJSON = genericParseJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase "pd"
      }


data Item = Item
  { itemName      :: !Text
  , itemQuantity  :: !Int
  , itemPrice     :: !Int
  , itemCategory  :: Maybe Text
  , itemUrl       :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item where
  toJSON = genericToJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase "item"
      }

instance FromJSON Item where
  parseJSON = genericParseJSON $
    xenditOptions
      { fieldLabelModifier = camelToSnakeCase "item"
      }
