{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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
  , AddressType(..)
  , Address(..)
  , Customer(..)
  , Fee(..)
  , PaymentDetail(..)
  , Item(..)
  , getAuth
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Data.Text.Encoding
import Servant.API
import GHC.Generics
import Xiswa.Utils
import Xendit.Config

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

data Status = PENDING | PAID | EXPIRED | SETTLED
  deriving (Eq, Show, Generic)

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

data EWallet =
    DANA
  | OVO
  | LINKAJA
  | SHOPEEPAY
  | GCASH
  | GRABPAY
  | PAYMAYA
  deriving (Eq, Show, Generic)

newtype RetailOutlet = RetailOutlet
  { retailOutletName    :: Text
  }
  deriving (Eq, Show, Generic)

newtype QRCode = QRCode
  { qrCodeType          :: Text
  }
  deriving (Eq, Show, Generic)

newtype DirectDebit = DirectDebit
  { directDebitType     :: Text
  }
  deriving (Eq, Show, Generic)

newtype PayLater = PayLater
  { paylaterType        :: Text
  }
  deriving (Eq, Show, Generic)

data AddressType =
    HOME
  | WORK
  | PROVINCIAL
  deriving (Eq, Show, Generic)

data Address = Address
  { addressCity        :: Maybe Text
  , addressCountry     :: !Text
  , addressPostalCode  :: Maybe Text
  , addressState       :: Maybe Text
  , addressStreetLine1 :: Maybe Text
  , addressStreetLine2 :: Maybe Text
  , addressCategory    :: Maybe AddressType
  , addressIsPrimary   :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

data Customer = Customer
  { customerGivenNames    :: Maybe Text
  , customerSurname       :: Maybe Text
  , customerEmail         :: Maybe Text
  , customerMobileNumber  :: Maybe Text
  , customerAddresses     :: Maybe [Address]
  }
  deriving (Eq, Show, Generic)

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

data Item = Item
  { itemName      :: !Text
  , itemQuantity  :: !Int
  , itemPrice     :: !Int
  , itemCategory  :: Maybe Text
  , itemUrl       :: Maybe Text
  }
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions {
    omitNothingFields = True  
  } ''Currency)

$(deriveJSON defaultOptions {
    omitNothingFields = True  
  } ''Status)

$(deriveJSON defaultOptions { 
    omitNothingFields = True  
  } ''BankCode)

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

$(deriveJSON defaultOptions {
    omitNothingFields = True  
  } ''EWallet)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase ""
  } ''RetailOutlet)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase ""
  } ''QRCode)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase ""
  } ''DirectDebit)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase ""
  } ''PayLater)

$(deriveJSON defaultOptions { 
    omitNothingFields = True  
  } ''AddressType)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase "address"
  } ''Address)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase "customer"
  } ''Customer)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase "pd"
  } ''PaymentDetail)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase "item"
  } ''Item)
