{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleInstances          #-}
module Main (main) where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe (fromJust)
import Data.Aeson
import Servant.Client
import Test.Hspec

import Xiswa.Utils

import Xendit

newtype TestEnv = TestEnv
  { unTestEnv :: XenditConfig
  }

instance Has XenditConfig TestEnv where
  obtain = unTestEnv

newtype Test a = Test
  { unTest :: ReaderT TestEnv IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv)

instance MonadError ClientError Test where
  throwError :: ClientError -> Test a
  throwError = liftIO . throwIO
  {-# INLINE throwError #-}

  catchError :: Test a -> (ClientError -> Test a) -> Test a
  catchError action handler = Test $ ReaderT $ \env -> do
    let ioAction = runTest env action
    ioAction `catch` \e -> runTest env $ handler e
  {-# INLINE catchError #-}

runTest :: TestEnv -> Test a -> IO a
runTest env = flip runReaderT env . unTest

spec :: TestEnv -> Spec
spec conf = do
  let address = Address
        { addressCountry     = "ID"
        , addressCity        = Just "Bantul"
        , addressCategory    = Just HOME
        , addressPostalCode  = Nothing
        , addressIsPrimary   = Nothing
        , addressState       = Nothing
        , addressStreetLine1 = Nothing
        , addressStreetLine2 = Nothing
        }

      customer = Customer
        { customerGivenNames   = Just "Wahyu"
        , customerEmail        = Just "archbung@gmail.com"
        , customerAddresses    = Just [address]
        , customerSurname      = Nothing
        , customerMobileNumber = Nothing
        }

      invoiceRequest = InvoiceRequest
        { invoiceReqExternalId          = "some-external-id"
        , invoiceReqAmount              = 3000
        , invoiceReqDescription         = "some-description"
        , invoiceReqSuccessRedirectUrl  = Nothing
        , invoiceReqFailureRedirectUrl  = Nothing
        , invoiceReqCustomer            = Just customer
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

  describe "getBalance" $ do
    it "correctly gets current balance" $ do
      -- Defaults to CASH
      cashBalance <- runTest conf $ getBalance id Nothing
      cashBalance' <- runTest conf $ getBalance id (Just CASH)
      cashBalance `shouldBe` cashBalance'
      

main :: IO ()
main = do
  xenditConfig <- fromJust <$> decodeFileStrict' "config.json"
  hspec $ spec (TestEnv xenditConfig)
