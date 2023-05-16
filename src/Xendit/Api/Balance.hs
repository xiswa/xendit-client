{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Xendit.Api.Balance (
    AccountType(..)
  , Balance(..)
  , getBalance
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson.TH
import Data.Text
import Data.Bifunctor
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Client.Generic
import GHC.Generics
import Xiswa.Utils
import Xendit.Config
import Xendit.Api.Common

newtype BalanceR route = BalanceR
  { _getBalance :: route
      :- "balance"
      :> BasicAuth "xendit" ()
      :> QueryParam "account_type" AccountType
      :> Get '[JSON] Balance
  }
  deriving (Generic)

data AccountType = CASH | HOLDING | TAX
  deriving (Eq, Show, Generic)

newtype Balance = Balance 
  { balance :: Int 
  }
  deriving (Eq, Show, Generic)

instance ToHttpApiData AccountType where
  toQueryParam = pack . show

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  } ''AccountType)

$(deriveJSON defaultOptions { 
    omitNothingFields  = True  
  } ''Balance)

{- | Automatically derive client functions -}
balanceRoutes 
  :: forall env err m. 
     ( MonadReader env m
     , Has XenditConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> BalanceR (AsClientT m)
balanceRoutes errorConv = genericClientHoist $ \c -> do
  XenditConfig{..} <- grab
  manager <- liftIO $ newManager tlsManagerSettings
  let env = mkClientEnv manager xenditApiUrl
  resp <- liftIO (runClientM c env)
  liftEither $ first errorConv resp

getBalance 
  :: forall env err m.
     ( MonadReader env m
     , Has XenditConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> Maybe AccountType 
  -> m Balance
getBalance errorConv maybeAccountType = do
  xenditAuth <- getAuth
  _getBalance (balanceRoutes errorConv) xenditAuth maybeAccountType
