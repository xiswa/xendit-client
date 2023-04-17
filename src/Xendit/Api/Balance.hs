{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
module Xendit.Api.Balance (
    AccountType(..)
  , Balance(..)
  , getBalance
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson
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

{- | Automatically derive client functions -}
balanceRoutes 
  :: forall env err m. 
     ( MonadReader env m
     , Has XenditConfig env
     , Has (ClientError -> err) env
     , MonadIO m
     , MonadError err m
     )
  => BalanceR (AsClientT m)
balanceRoutes = genericClientHoist $ \c -> do
  XenditConfig{..} <- grab
  errorConv <- grab
  manager <- liftIO $ newManager tlsManagerSettings
  let env = mkClientEnv manager xenditApiUrl
  resp <- liftIO (runClientM c env)
  liftEither $ first errorConv resp

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
  _getBalance balanceRoutes xenditAuth maybeAccountType


data AccountType = CASH | HOLDING | TAX
  deriving (Eq, Show, Generic)

instance ToHttpApiData AccountType where
  toQueryParam = pack . show

instance ToJSON AccountType where
  toJSON = genericToJSON xenditOptions

instance FromJSON AccountType where
  parseJSON = genericParseJSON xenditOptions


newtype Balance = Balance 
  { balance :: Int 
  }
  deriving (Eq, Show, Generic)

instance ToJSON Balance where
  toJSON = genericToJSON xenditOptions

instance FromJSON Balance where
  parseJSON = genericParseJSON xenditOptions
