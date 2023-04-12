{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Xendit.Types (
  -- Xendit datatypes
    AccountType(..)
  , Balance(..)
   ,Currency(..)
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
  , InvoiceRequest(..)
  , InvoiceResponse(..)
  , PaymentDetail(..)
  , Item(..)
  , InvoicePayload(..)
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Servant.API (ToHttpApiData(..))

import Xiswa.Utils


data AccountType = CASH | HOLDING | TAX
  deriving (Eq, Show)

instance ToHttpApiData AccountType where
  toQueryParam = pack . show

$(deriveJSON defaultOptions {
    omitNothingFields = True
  } ''AccountType)

newtype Balance = Balance 
  { balance :: Int 
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  } ''Balance)

data Currency = IDR | PHP
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  } ''Currency)

data Status = PENDING | PAID | EXPIRED | SETTLED
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  } ''Status)

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
  deriving (Eq, Show)

$(deriveJSON defaultOptions { omitNothingFields = True } ''BankCode)

-- | A simplified Xendit bank object
-- Most fields are encoded as Text for simplicity.
data Bank = Bank
  { bankCode              :: !BankCode
  , bankCollectionType    :: !Text
  , bankTransferAmount    :: !Int
  , bankBranch            :: !Text
  , bankAccountHolderName :: !Text
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  } ''EWallet)

newtype RetailOutlet = RetailOutlet
  { retailOutletName    :: Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  , fieldLabelModifier = camelToSnake
  } ''RetailOutlet)

newtype QRCode = QRCode
  { qrCodeType          :: Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  , fieldLabelModifier = camelToSnake
  } ''QRCode)

newtype DirectDebit = DirectDebit
  { directDebitType     :: Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  , fieldLabelModifier = camelToSnake} ''DirectDebit)

newtype PayLater = PayLater
  { paylaterType        :: Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  , fieldLabelModifier = camelToSnake
  } ''PayLater)

data Customer = Customer
  { customerGivenNames    :: Maybe Text
  , customerSurname       :: Maybe Text
  , customerEmail         :: Maybe Text
  , customerMobileNumber  :: Maybe Text
  , customerAddresses     :: Maybe Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  , fieldLabelModifier = camelToSnakeWithPref "customer"
  } ''Customer)

data Fee = Fee
  { typ     :: Text
  , value   :: Int
  }
  deriving (Eq, Show)

instance ToJSON Fee where
  toJSON (Fee ty val) =
    object [ "type"  .= ty
           , "value" .= val
           ]

instance FromJSON Fee where
  parseJSON = withObject "Fee" $ \obj ->
    Fee <$> obj .: "type"
        <*> obj .: "value"

-- | A simplified Xendit invoice creation object
-- https://developers.xendit.co/api-reference/?python#create-invoice
data InvoiceRequest = InvoiceRequest
  { invoiceReqExternalId          :: !Text
  , invoiceReqAmount              :: !Int
  , invoiceReqDescription         :: !Text
  , invoiceReqSuccessRedirectUrl  :: Maybe Text
  , invoiceReqFailureRedirectUrl  :: Maybe Text
  , invoiceReqCustomer            :: Maybe Customer 
  , invoiceReqFees                :: Maybe [Fee]
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  , fieldLabelModifier = camelToSnakeWithPref "invoiceReq"
  } ''InvoiceRequest)

-- | A simplified Xendit invoice creation response object
-- https://developers.xendit.co/api-reference/?python#create-invoice
data InvoiceResponse = InvoiceResponse
  { invoiceRespId                         :: !Text 
  , invoiceRespExternalId                 :: !Text
  , invoiceRespUserId                     :: !Text
  , invoiceRespStatus                     :: !Status
  , invoiceRespMerchantName               :: !Text
  , invoiceRespMerchantProfilePictureUrl  :: !Text
  , invoiceRespAmount                     :: !Int
  , invoiceRespDescription                :: !Text
  , invoiceRespExpiryDate                 :: !UTCTime
  , invoiceRespInvoiceUrl                 :: !Text
  , invoiceRespAvailableBanks             :: [Bank]
  , invoiceRespAvailableRetailOutlets     :: [RetailOutlet]
  , invoiceRespAvailableQrCodes           :: [QRCode]
  , invoiceRespAvailableDirectDebits      :: [DirectDebit]
  , invoiceRespAvailablePaylaters         :: [PayLater]
  , invoiceRespShouldExcludeCreditCard    :: !Bool
  , invoiceRespShouldSendEmail            :: !Bool
  , invoiceRespCreated                    :: !UTCTime
  , invoiceRespUpdated                    :: !UTCTime
  , invoiceRespCurrency                   :: !Currency
  , invoiceRespCustomer                   :: Maybe Customer
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  , fieldLabelModifier = camelToSnakeWithPref "invoiceResp"
  } ''InvoiceResponse)

-- | Object containing payment details
-- Currently supporting QRIS only
data PaymentDetail = PaymentDetail
  { pdReceiptId   :: !Text
  , pdSource      :: !Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  , fieldLabelModifier = camelToSnakeWithPref "pd"
  } ''PaymentDetail)

data Item = Item
  { itemName      :: !Text
  , itemQuantity  :: !Int
  , itemPrice     :: !Int
  , itemCategory  :: Maybe Text
  , itemUrl       :: Maybe Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True 
  , fieldLabelModifier = camelToSnakeWithPref "item"
  } ''Item)

-- A Xendit invoice callback payload object
-- https://developers.xendit.co/api-reference/#invoice-callback
data InvoicePayload = InvoicePayload
  { invoicePayloadId                              :: !Text
  , invoicePayloadExternalId                      :: !Text
  , invoicePayloadUserId                          :: !Text
  , invoicePayloadIsHigh                          :: !Bool
  , invoicePayloadStatus                          :: !Status
  , invoicePayloadMerchantName                    :: !Text
  , invoicePayloadAmount                          :: !Int
  , invoicePayloadPayerEmail                      :: Maybe Text
  , invoicePayloadDescription                     :: !Text
  , invoicePayloadPaidAmount                      :: Maybe Int
  , invoicePayloadAdjustedReceivedAmount          :: Maybe Int
  , invoicePayloadUpdated                         :: !UTCTime
  , invoicePayloadCreated                         :: !UTCTime
  , invoicePayloadCurrency                        :: !Text
  , invoicePayloadPaidAt                          :: Maybe UTCTime
  , invoicePayloadPaymentMethod                   :: Maybe Text
  , invoicePayloadPaymentChannel                  :: Maybe Text
  , invoicePayloadpaymentDestination              :: Maybe Text
  , invoicePayloadPaymentDetails                  :: Maybe PaymentDetail
  , invoicePayloadPaymentId                       :: Maybe Text
  , invoicePayloadSuccessRedirectUrl              :: Maybe Text
  , invoicePayloadFailureRedirectUrl              :: Maybe Text
  , invoicePayloadCreditCardChargeId              :: Maybe Text
  , invoicePayloadItems                           :: Maybe [Item]
  , invoicePayloadFees                            :: Maybe [Fee]
  , invoicePayloadShouldAuthenticateCreditCard    :: Maybe Bool
  , invoicePayloadBankCode                        :: Maybe BankCode
  , invoicePayloadEwalletType                     :: Maybe EWallet
  , invoicePayloadOnDemandLink                    :: Maybe Text
  , invoicePayloadRecurringPaymentId              :: Maybe Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    omitNothingFields = True
  , fieldLabelModifier = camelToSnakeWithPref "invoicePayload"
  } ''InvoicePayload)
