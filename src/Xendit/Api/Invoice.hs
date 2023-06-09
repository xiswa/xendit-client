{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell  #-}
module Xendit.Api.Invoice (
    InvoiceRequest(..)
  , InvoiceResponse(..)
  , InvoicePayload(..)
  , requestInvoice
  , getInvoice
  , getAllInvoices
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson.TH
import Data.Time
import Data.Bifunctor
import Data.Text
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Client.Generic
import Xiswa.Utils
import Xendit.Config
import Xendit.Api.Common

data InvoiceR route = InvoiceR
  {
    _requestInvoice :: route
      :- "v2" 
      :> "invoices"
      :> BasicAuth "xendit" ()
      :> ReqBody '[JSON] InvoiceRequest 
      :> Post '[JSON] InvoiceResponse

  , _getAllInvoices :: route
      :- "v2" 
      :> "invoices"
      :> BasicAuth "xendit" ()
      :> Get '[JSON] [InvoiceResponse]

  , _getInvoice :: route
      :- "v2" 
      :> "invoices"
      :> BasicAuth "xendit" ()
      :> Capture "id" Text
      :> Get '[JSON] InvoiceResponse
  }
  deriving (Generic)

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
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase "invoiceReq"
  } ''InvoiceRequest)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase "invoiceResp"
  } ''InvoiceResponse)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  , fieldLabelModifier = camelToSnakeCase "invoicePayload"
  } ''InvoicePayload)

{- | Automatically derive client functions -}
invoiceRoutes 
  :: forall env err m. 
     ( MonadReader env m
     , Has XenditConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> InvoiceR (AsClientT m)
invoiceRoutes errorConv = genericClientHoist $ \c -> do
  XenditConfig{..} <- grab
  manager <- liftIO $ newManager tlsManagerSettings
  let env = mkClientEnv manager xenditApiUrl
  resp <- liftIO (runClientM c env)
  liftEither $ first errorConv resp
  
requestInvoice 
  :: forall env err m.
     ( MonadReader env m
     , Has XenditConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> InvoiceRequest 
  -> m InvoiceResponse
requestInvoice errorConv invoiceRequest = do
  xenditAuth <- getAuth
  _requestInvoice (invoiceRoutes errorConv) xenditAuth invoiceRequest

getAllInvoices 
  :: forall env err m.
     ( MonadReader env m
     , Has XenditConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> m [InvoiceResponse]
getAllInvoices errorConv = do
  xenditAuth <- getAuth
  _getAllInvoices (invoiceRoutes errorConv) xenditAuth

getInvoice
  :: forall env err m.
     ( MonadReader env m
     , Has XenditConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> Text 
  -> m InvoiceResponse
getInvoice errorConv invoiceId = do 
  xenditAuth <- getAuth
  _getInvoice (invoiceRoutes errorConv) xenditAuth invoiceId
