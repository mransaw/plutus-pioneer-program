{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Data.Void             as Void
import Control.Monad.Freer.Extras as Extras
import Wallet.Emulator.Wallet
import Control.Monad

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    Contract.handleError (\err -> Contract.logError $ "caught: " ++ unpack err) $ void $ submitTx tx
    -- the void is needed to discard the result of submitTx
    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace i1 i2 = do
    h <- activateContractWallet (Wallet 1) payContract
    let p = PayParams 
            { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
            , ppLovelace = i1
            }
    callEndpoint @"pay" h p
    void $ Emulator.waitNSlots 1
    --- turn this into a Monad?
    
    let p = PayParams 
            { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
            , ppLovelace = i2
            }
    callEndpoint @"pay" h p
    void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
