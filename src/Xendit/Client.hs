{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Xendit.Client (
  -- Constraints
    HasXenditConfig(..)
  , XenditConfig(..)
  , WithXendit

  -- Client functions
  , getBalance
  , requestInvoice
  , getInvoice
  , getAllInvoices
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson.TH
import Data.Text (Text)
import Data.Text.Encoding
import Data.Bifunctor (first)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Client.Generic
import GHC.Generics

import Xendit.Types
import Xendit.Internal.Utils

class HasXenditConfig env where
  obtain :: env -> XenditConfig

data XenditConfig = XenditConfig
  { xenditApiKey        :: !Text
  , xenditApiUrl        :: !BaseUrl
  , xenditCallbackToken :: !Text
  }
  deriving (Eq, Show)

$(deriveJSON xenditOptions {fieldLabelModifier = camelWithPrefToSnake "xendit"} ''XenditConfig)

instance HasXenditConfig XenditConfig where
  obtain = id

type WithXendit env err m = 
  ( MonadReader env m
  , HasXenditConfig env
  , MonadIO m
  , MonadError err m
  )

-- | Xendit API endpoints
-- https://api.xendit.co/v2/
data Xendit route = Xendit
  { 
    _getBalance :: route
      :- "balance"
      :> BasicAuth "xendit" ()
      :> QueryParam "account_type" AccountType
      :> Get '[JSON] Balance

  , _requestInvoice :: route
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

{- | Automatically derive client functions -}
xenditRoutes 
  :: forall env err m. (WithXendit env err m)
  => (ClientError -> err)
  -> Xendit (AsClientT m)
xenditRoutes errorConv = genericClientHoist $ \c -> do
  XenditConfig{..} <- asks obtain
  manager <- liftIO $ newManager tlsManagerSettings
  let env = mkClientEnv manager xenditApiUrl
  resp <- liftIO (runClientM c env)
  liftEither $ first errorConv resp

getAuth
  :: forall env err m. (WithXendit env err m)
  => m BasicAuthData  
getAuth = do
  XenditConfig{..} <- asks obtain
  return $ BasicAuthData (encodeUtf8 xenditApiKey) ""

getBalance 
  :: forall env err m. (WithXendit env err m)
  => (ClientError -> err)
  -> Maybe AccountType 
  -> m Balance
getBalance errorConv maybeAccountType = do
  xenditAuth <- getAuth
  _getBalance (xenditRoutes errorConv) xenditAuth maybeAccountType
  
requestInvoice 
  :: forall env err m. (WithXendit env err m)
  => (ClientError -> err)
  -> InvoiceRequest 
  -> m InvoiceResponse
requestInvoice errorConv invoiceRequest = do
  xenditAuth <- getAuth
  _requestInvoice (xenditRoutes errorConv) xenditAuth invoiceRequest

getAllInvoices 
  :: forall env err m. (WithXendit env err m)
  => (ClientError -> err)
  -> m [InvoiceResponse]
getAllInvoices errorConv = do
  xenditAuth <- getAuth
  _getAllInvoices (xenditRoutes errorConv) xenditAuth

getInvoice
  :: forall env err m. (WithXendit env err m)
  => (ClientError -> err)
  -> Text 
  -> m InvoiceResponse
getInvoice errorConv invoiceId = do 
  xenditAuth <- getAuth
  _getInvoice (xenditRoutes errorConv) xenditAuth invoiceId
