{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Xendit.Client (
    XenditConfig(..)

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

import Xiswa.Utils

import Xendit.Types

data XenditConfig = XenditConfig
  { xenditApiKey        :: !Text
  , xenditApiUrl        :: !BaseUrl
  , xenditCallbackToken :: !Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier = camelToSnakeWithPref "xendit"
  } ''XenditConfig)

-- | Xendit API endpoints
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
  :: forall env err m. 
     ( MonadReader env m
     , Has XenditConfig env
     , Has (ClientError -> err) env
     , MonadIO m
     , MonadError err m
     )
  => Xendit (AsClientT m)
xenditRoutes = genericClientHoist $ \c -> do
  XenditConfig{..} <- grab
  errorConv <- grab
  manager <- liftIO $ newManager tlsManagerSettings
  let env = mkClientEnv manager xenditApiUrl
  resp <- liftIO (runClientM c env)
  liftEither $ first errorConv resp

getAuth
  :: forall env m.
     ( MonadReader env m
     , Has XenditConfig env
     )
  => m BasicAuthData  
getAuth = do
  XenditConfig{..} <- grab
  return $ BasicAuthData (encodeUtf8 xenditApiKey) ""

getBalance 
  :: forall env err m.
     ( MonadReader env m
     , Has XenditConfig env
     , Has (ClientError -> err) env
     , MonadIO m
     , MonadError err m
     )
  => Maybe AccountType 
  -> m Balance
getBalance maybeAccountType = do
  xenditAuth <- getAuth
  _getBalance xenditRoutes xenditAuth maybeAccountType
  
requestInvoice 
  :: forall env err m.
     ( MonadReader env m
     , Has XenditConfig env
     , Has (ClientError -> err) env
     , MonadIO m
     , MonadError err m
     )
  => InvoiceRequest 
  -> m InvoiceResponse
requestInvoice invoiceRequest = do
  xenditAuth <- getAuth
  _requestInvoice xenditRoutes xenditAuth invoiceRequest

getAllInvoices 
  :: forall env err m.
     ( MonadReader env m
     , Has XenditConfig env
     , Has (ClientError -> err) env
     , MonadIO m
     , MonadError err m
     )
  => m [InvoiceResponse]
getAllInvoices = do
  xenditAuth <- getAuth
  _getAllInvoices xenditRoutes xenditAuth

getInvoice
  :: forall env err m.
     ( MonadReader env m
     , Has XenditConfig env
     , Has (ClientError -> err) env
     , MonadIO m
     , MonadError err m
     )
  => Text 
  -> m InvoiceResponse
getInvoice invoiceId = do 
  xenditAuth <- getAuth
  _getInvoice xenditRoutes xenditAuth invoiceId
