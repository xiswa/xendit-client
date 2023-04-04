{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
module Main (main) where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe (fromJust)
import Data.Aeson
import Servant.Client
import Test.Hspec

import Xendit

newtype Test a = Test
  { unTest :: ReaderT XenditConfig IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader XenditConfig)

instance MonadError ClientError Test where
  throwError :: ClientError -> Test a
  throwError = liftIO . throwIO
  {-# INLINE throwError #-}

  catchError :: Test a -> (ClientError -> Test a) -> Test a
  catchError action handler = Test $ ReaderT $ \env -> do
    let ioAction = runTest env action
    ioAction `catch` \e -> runTest env $ handler e
  {-# INLINE catchError #-}

runTest :: XenditConfig -> Test a -> IO a
runTest env = flip runReaderT env . unTest

spec :: XenditConfig -> Spec
spec conf = do
  let invoiceRequest = InvoiceRequest
        { invoiceReqExternalId          = "some-external-id"
        , invoiceReqAmount              = 3000
        , invoiceReqDescription         = "some-description"
        , invoiceReqSuccessRedirectUrl  = Nothing
        , invoiceReqFailureRedirectUrl  = Nothing
        , invoiceReqCustomer            = Nothing
        , invoiceReqFees                = Nothing
        }

  describe "requestInvoice" $ do
    it "responds with correct fields" $ do
      invoiceResp <- runTest conf $ requestInvoice id invoiceRequest
      invoiceRespExternalId invoiceResp `shouldBe` invoiceReqExternalId invoiceRequest
      invoiceRespDescription invoiceResp `shouldBe` invoiceReqDescription invoiceRequest
      invoiceRespAmount invoiceResp `shouldBe` invoiceReqAmount invoiceRequest

  describe "getInvoice" $ do
    it "correctly gets invoice by id" $ do
      invoiceResp1 <- runTest conf $ requestInvoice id invoiceRequest
      invoiceResp2 <- runTest conf $ getInvoice id (invoiceRespId invoiceResp1)
      invoiceResp2 `shouldBe` invoiceResp1
      

main :: IO ()
main = do
  xenditConfig <- fromJust <$> decodeFileStrict' "config.json"
  hspec $ spec xenditConfig
