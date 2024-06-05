{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
--{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

module Spec.AgdaFeatures where

import Cardano.Api qualified as C
import Cardano.Api.Address as C
import Cardano.Ledger.Plutus.TxInfo
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Time.Clock qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog qualified as H
import Hedgehog.Internal.Property (MonadTest)
import Helpers.Common (makeAddress, toShelleyBasedEra)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.TypeConverters as TC
import Helpers.Utils qualified as U
import PlutusLedgerApi.V1.Address as P
import PlutusLedgerApi.V1.Interval qualified as P
import PlutusLedgerApi.V1.Time qualified as P
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V1.Value qualified as P
import PlutusScripts.Agda.Common as SM
import PlutusScripts.Agda.SM as SM
import PlutusScripts.Basic.V_1_0 qualified as PS_1_0
import PlutusScripts.Basic.V_1_1 qualified as PS_1_1
import PlutusTx.Builtins qualified as BI

import PlutusScripts.Helpers qualified as PS
import PlutusScripts.V2TxInfo qualified as PS (
  checkV2TxInfoAssetIdV2,
  checkV2TxInfoMintWitnessV2,
  checkV2TxInfoRedeemer,
  txInfoData,
  txInfoFee,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoSigs,
 )



addInHoldingFailTestInfo =
  TestInfo
    { testName = "addInHoldingFailTest"
    , testDescription =
        "Can't Add in Holding state"
    , test = addInHoldingFailTest
    }

addInHoldingFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
addInHoldingFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Add w1Pkh)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w1Pkh]) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent2
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx


payInHoldingFailTestInfo =
  TestInfo
    { testName = "payInHoldingFailTest"
    , testDescription =
        "Can't Pay in Holding state"
    , test = payInHoldingFailTest
    }

payInHoldingFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
payInHoldingFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Pay)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent2
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

cancelInHoldingFailTestInfo =
  TestInfo
    { testName = "cancelInHoldingFailTest"
    , testDescription =
        "Can't Cancel in Holding state"
    , test = addInHoldingFailTest
    }

cancelInHoldingFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
cancelInHoldingFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Cancel)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w1Pkh]) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent2
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx


proposePayoutValueTooLargeFailTestInfo =
  TestInfo
    { testName = "proposePayoutValueTooLargeFailTest"
    , testDescription =
        "Propose fails when datum has too much value in it "
    , test = proposePayoutValueTooLargeFailTest
    }

proposePayoutValueTooLargeFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
proposePayoutValueTooLargeFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId

  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"


  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 14200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 14200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent2
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

proposeResultsInHoldingFailTestInfo =
  TestInfo
    { testName = "proposeResultsInHoldingFailTest"
    , testDescription =
        "Propose can't result in holding"
    , test = proposeResultsInHoldingFailTest
    }

proposeResultsInHoldingFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
proposeResultsInHoldingFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId

  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"


  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 14200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent2
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx


proposeInconsistentDatumValueFailTestInfo =
  TestInfo
    { testName = "proposeInconsistentDatumValueFailTest"
    , testDescription =
        "Propose redeemer and datum have different value"
    , test = proposeInconsistentDatumValueFailTest
    }

proposeInconsistentDatumValueFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
proposeInconsistentDatumValueFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId

  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"


  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4000000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent2
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

proposeInconsistentDatumTargetFailTestInfo =
  TestInfo
    { testName = "proposeInconsistentDatumTargetFailTest"
    , testDescription =
        "Propose redeemer and datum have different Target"
    , test = proposeInconsistentDatumTargetFailTest
    }

proposeInconsistentDatumTargetFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
proposeInconsistentDatumTargetFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId

  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"


  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w2Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent2
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

proposeInconsistentDatumDeadlineFailTestInfo =
  TestInfo
    { testName = "proposeInconsistentDatumDeadlineFailTest"
    , testDescription =
        "Propose redeemer and datum have different deadline"
    , test = proposeInconsistentDatumDeadlineFailTest
    }

proposeInconsistentDatumDeadlineFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
proposeInconsistentDatumDeadlineFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId

  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"


  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w2Pkh 1500 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent2
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx


proposeInconsistentDatumSigsFailTestInfo =
  TestInfo
    { testName = "proposeInconsistentDatumSigsFailTest"
    , testDescription =
        "Propose redeemer and datum have different sigs"
    , test = proposeInconsistentDatumSigsFailTest
    }

proposeInconsistentDatumSigsFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
proposeInconsistentDatumSigsFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId

  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"


  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w1Pkh]) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent2
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

addUnsignedSignatureFailTestInfo =
  TestInfo
    { testName = "addUnsigedSignatureFailTest"
    , testDescription =
        "Can't Add signatures that haven't actually signed the transaction"
    , test = addUnsignedSignatureFailTest
    }

addUnsignedSignatureFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
addUnsignedSignatureFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Addr

  let
    -- without reference script
    scriptTxIn2 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)
    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn2] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral1
        , C.txOuts = [scriptTxOut3]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent3
        w1Addr
        Nothing
        [C.WitnessPaymentKey w1SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation" 
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

addWrongSignatureFailTestInfo =
  TestInfo
    { testName = "addWrongSignatureFailTest"
    , testDescription =
        "Can't Add signatures that are now authorized"
    , test = addWrongSignatureFailTest
    }

addWrongSignatureFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
addWrongSignatureFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Addr

  let
    -- without reference script
    scriptTxIn2 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w1Pkh)
                 sbe Nothing Nothing exUnits)
    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w1Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn2] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral1
        , C.txOuts = [scriptTxOut3]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent3
        w1Addr
        Nothing
        [C.WitnessPaymentKey w1SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx



addResultsInHoldingFailTestInfo =
  TestInfo
    { testName = "addResultsInHoldingFailTest"
    , testDescription =
        "Can't Add and end up in Holding state"
    , test = addResultsInHoldingFailTest
    }

addResultsInHoldingFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
addResultsInHoldingFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn2 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)
    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn2] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent3
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation" 
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx


addInconsistentDatumValueFailTestInfo =
  TestInfo
    { testName = "addInconsistentDatumValueFailTest"
    , testDescription =
        "Can't Add if old and new Datum have inconsistent value"
    , test = addInconsistentDatumValueFailTest
    }

addInconsistentDatumValueFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
addInconsistentDatumValueFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn2 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)
    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4000000) w1Pkh 1000 [w2Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn2] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent3
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation" 
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx


addInconsistentDatumTargetFailTestInfo =
  TestInfo
    { testName = "addInconsistentDatumTargetFailTest"
    , testDescription =
        "Can't Add if old and new Datum have inconsistent target"
    , test = addInconsistentDatumTargetFailTest
    }

addInconsistentDatumTargetFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
addInconsistentDatumTargetFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn2 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)
    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4000000) w2Pkh 1000 [w2Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn2] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent3
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation" 
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

addInconsistentDatumDeadlineFailTestInfo =
  TestInfo
    { testName = "addInconsistentDatumDeadlineFailTest"
    , testDescription =
        "Can't Add if old and new Datum have inconsistent deadline"
    , test = addInconsistentDatumDeadlineFailTest
    }

addInconsistentDatumDeadlineFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
addInconsistentDatumDeadlineFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn2 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)
    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4000000) w1Pkh 1500 [w2Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn2] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent3
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation" 
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx


addInconsistentDatumSigsFailTestInfo =
  TestInfo
    { testName = "addInconsistentDatumSigsFailTest"
    , testDescription =
        "Can't Add if old and new Datum have inconsistent sigs"
    , test = addInconsistentDatumSigsFailTest
    }

addInconsistentDatumSigsFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
addInconsistentDatumSigsFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn2 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)
    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4000000) w1Pkh 1000 [w2Pkh, w3Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn2] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent3
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation" 
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx


payWithoutEnoughSignaturesFailTestInfo =
  TestInfo
    { testName = "payWithoutEnoughSignaturesFailTest"
    , testDescription =
        "Paying can't happen without enough sigs "
    , test = payWithoutEnoughSignaturesFailTest
    }

payWithoutEnoughSignaturesFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
payWithoutEnoughSignaturesFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn3 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn3] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w2VKey]
        }

  signedTx3 <- Tx.buildTx era localNodeConnectInfo txBodyContent3 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx3
  let txInAtScript3 = Tx.txIn (Tx.txId signedTx3) 0
  
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "waitForTxInAtAddress"

  
  txIn5 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Addr

  let
    scriptTxIn5 = Tx.txInWitness txInAtScript3 (SM.smSpendWitness par
                 (Pay)
                 sbe Nothing Nothing exUnits)

    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut5 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 5_800_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , 
            tToken = tt }))
    paymentTxOut = Tx.txOut era (C.lovelaceToValue 4_200_000) w1Addr

    txBodyContent5 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn5] ++ (Tx.pubkeyTxIns [txIn5])
        , C.txInsCollateral = collateral1
        , C.txOuts = [scriptTxOut5, paymentTxOut]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent5
        w1Addr
        Nothing
        [C.WitnessPaymentKey w1SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

payResultsInCollectingFailTestInfo =
  TestInfo
    { testName = "payResultsInCollectingFailTest"
    , testDescription =
        "Paying can't result in a Collecting state "
    , test = payResultsInCollectingFailTest
    }


payResultsInCollectingFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
payResultsInCollectingFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn3 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn3] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w2VKey]
        }

  signedTx3 <- Tx.buildTx era localNodeConnectInfo txBodyContent3 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx3
  let txInAtScript3 = Tx.txIn (Tx.txId signedTx3) 0
  
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "waitForTxInAtAddress"


  txIn4 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w3Addr

  let
    collateral3 = Tx.txInsCollateral era [otherTxIn3] 
    scriptTxIn4 = Tx.txInWitness txInAtScript3 (SM.smSpendWitness par
                 (Add w3Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut4 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh, w3Pkh]) , 
            tToken = tt }))

    txBodyContent4 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn4] ++ (Tx.pubkeyTxIns [txIn4])
        , C.txInsCollateral = collateral3
        , C.txOuts = [scriptTxOut4]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w3VKey]
        }

  signedTx4 <- Tx.buildTx era localNodeConnectInfo txBodyContent4 w3Addr w3SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx4
  let txInAtScript4 = Tx.txIn (Tx.txId signedTx4) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript4 "waitForTxInAtAddress"
  
  txIn5 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Addr

  let
    scriptTxIn5 = Tx.txInWitness txInAtScript4 (SM.smSpendWitness par
                 (Pay)
                 sbe Nothing Nothing exUnits)

    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut5 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 5_800_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh, w3Pkh]) , 
            tToken = tt }))
    paymentTxOut = Tx.txOut era (C.lovelaceToValue 4_200_000) w1Addr

    txBodyContent5 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn5] ++ (Tx.pubkeyTxIns [txIn5])
        , C.txInsCollateral = collateral1
        , C.txOuts = [scriptTxOut5, paymentTxOut]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent5
        w1Addr
        Nothing
        [C.WitnessPaymentKey w1SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

payWithoutPayoutFailTestInfo =
  TestInfo
    { testName = "payWithoutPayoutFailTest"
    , testDescription =
        "Paying can't happen without the payout output "
    , test = payWithoutPayoutFailTest
    }

payWithoutPayoutFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
payWithoutPayoutFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn3 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn3] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w2VKey]
        }

  signedTx3 <- Tx.buildTx era localNodeConnectInfo txBodyContent3 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx3
  let txInAtScript3 = Tx.txIn (Tx.txId signedTx3) 0
  
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "waitForTxInAtAddress"


  txIn4 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w3Addr

  let
    collateral3 = Tx.txInsCollateral era [otherTxIn3] 
    scriptTxIn4 = Tx.txInWitness txInAtScript3 (SM.smSpendWitness par
                 (Add w3Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut4 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh, w3Pkh]) , 
            tToken = tt }))

    txBodyContent4 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn4] ++ (Tx.pubkeyTxIns [txIn4])
        , C.txInsCollateral = collateral3
        , C.txOuts = [scriptTxOut4]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w3VKey]
        }

  signedTx4 <- Tx.buildTx era localNodeConnectInfo txBodyContent4 w3Addr w3SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx4
  let txInAtScript4 = Tx.txIn (Tx.txId signedTx4) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript4 "waitForTxInAtAddress"
  
  txIn5 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Addr

  let
    scriptTxIn5 = Tx.txInWitness txInAtScript4 (SM.smSpendWitness par
                 (Pay)
                 sbe Nothing Nothing exUnits)

    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut5 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 5_800_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , 
            tToken = tt }))
    --paymentTxOut = Tx.txOut era (C.lovelaceToValue 4_200_000) w1Addr

    txBodyContent5 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn5] ++ (Tx.pubkeyTxIns [txIn5])
        , C.txInsCollateral = collateral1
        , C.txOuts = [scriptTxOut5] --, paymentTxOut]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent5
        w1Addr
        Nothing
        [C.WitnessPaymentKey w1SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

payWithoutUpdatingValueFailTestInfo =
  TestInfo
    { testName = "payWithoutUpdatingValueFailTest"
    , testDescription =
        "Paying can't happen without updating the value "
    , test = payWithoutUpdatingValueFailTest
    }

payWithoutUpdatingValueFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
payWithoutUpdatingValueFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn3 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn3] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w2VKey]
        }

  signedTx3 <- Tx.buildTx era localNodeConnectInfo txBodyContent3 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx3
  let txInAtScript3 = Tx.txIn (Tx.txId signedTx3) 0
  
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "waitForTxInAtAddress"


  txIn4 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w3Addr

  let
    collateral3 = Tx.txInsCollateral era [otherTxIn3] 
    scriptTxIn4 = Tx.txInWitness txInAtScript3 (SM.smSpendWitness par
                 (Add w3Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut4 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh, w3Pkh]) , 
            tToken = tt }))

    txBodyContent4 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn4] ++ (Tx.pubkeyTxIns [txIn4])
        , C.txInsCollateral = collateral3
        , C.txOuts = [scriptTxOut4]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w3VKey]
        }

  signedTx4 <- Tx.buildTx era localNodeConnectInfo txBodyContent4 w3Addr w3SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx4
  let txInAtScript4 = Tx.txIn (Tx.txId signedTx4) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript4 "waitForTxInAtAddress"
  
  txIn5 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Addr

  let
    scriptTxIn5 = Tx.txInWitness txInAtScript4 (SM.smSpendWitness par
                 (Pay)
                 sbe Nothing Nothing exUnits)

    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut5 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , 
            tToken = tt }))
    paymentTxOut = Tx.txOut era (C.lovelaceToValue 4_200_000) w1Addr

    txBodyContent5 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn5] ++ (Tx.pubkeyTxIns [txIn5])
        , C.txInsCollateral = collateral1
        , C.txOuts = [scriptTxOut5, paymentTxOut]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent5
        w1Addr
        Nothing
        [C.WitnessPaymentKey w1SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx




cancelBeforeDeadlineFailTestInfo =
  TestInfo
    { testName = "cancelBeforeDeadlineFailTest"
    , testDescription =
        "Can't Cancel before deadline"
    , test = cancelBeforeDeadlineFailTest
    }

cancelBeforeDeadlineFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
cancelBeforeDeadlineFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  startTime <- liftIO Time.getPOSIXTime

  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr



      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh (U.posixToMilliseconds startTime + 60_000))
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 
                                            (U.posixToMilliseconds startTime + 60_000) []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn3 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Cancel)
                 sbe Nothing Nothing exUnits)

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn3] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        , C.txValidityLowerBound = Tx.txValidityLowerBound era 1700
        , C.txValidityUpperBound = Tx.txValidityUpperBound era 2000
          -- \^ ~9min range (200ms slots)
          -- \^ Babbage era onwards cannot have upper slot beyond epoch boundary (10_000 slot epoch)
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w2VKey]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent3
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx



cancelAfterDeadlineTestInfo =
  TestInfo
    { testName = "cancelAfterDeadlineTest"
    , testDescription =
        "Can Cancel after deadline"
    , test = cancelAfterDeadlineTest
    }

cancelAfterDeadlineTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
cancelAfterDeadlineTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  startTime <- liftIO Time.getPOSIXTime

  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr



      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh ((U.posixToMilliseconds startTime) + 20))
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 
                                            ((U.posixToMilliseconds startTime) + 20) []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  slotNo <- Q.getCurrentSlotNo era localNodeConnectInfo
  time <- Q.waitForPOSIXTime era localNodeConnectInfo "Waiting for POSIX" (startTime + 30)
  slotNo2 <- Q.getCurrentSlotNo era localNodeConnectInfo

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr
  
  let
    -- without reference script
    scriptTxIn3 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Cancel)
                 sbe Nothing Nothing exUnits)

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn3] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        , C.txValidityLowerBound = Tx.txValidityLowerBound era (slotNo2 - 100)
        , C.txValidityUpperBound = Tx.txValidityUpperBound era (slotNo2 + 200)
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w2VKey]
        }
--{invalidBefore = SJust (SlotNo 624), invalidHereafter = SJust (SlotNo 1124)}) (SlotNo 617))
--{invalidBefore = SJust (SlotNo 617), invalidHereafter = SJust (SlotNo 1117)}) (SlotNo 614)
-- ++100
--{invalidBefore = SJust (SlotNo 720), invalidHereafter = SJust (SlotNo 1120)}) (SlotNo 621))
--{invalidBefore = SJust (SlotNo 720), invalidHereafter = SJust (SlotNo 1120)}) (SlotNo 564))
--{invalidBefore = SJust (SlotNo 717), invalidHereafter = SJust (SlotNo 1117)}) (SlotNo 585))
  signedTx3 <- Tx.buildTx era localNodeConnectInfo txBodyContent3 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx3
  let txInAtScript3 = Tx.txIn (Tx.txId signedTx3) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "waitForTxInAtAddress"

  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 10_000_000 <> tokenValue)
  assert "txOut has tokens" txOutHasAdaValue


cancelResultsInCollectingFailTestInfo =
  TestInfo
    { testName = "cancelResultsInCollectingFailTest"
    , testDescription =
        "Can't Cancel and end up in Collecting"
    , test = cancelResultsInCollectingFailTest
    }

cancelResultsInCollectingFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
cancelResultsInCollectingFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  startTime <- liftIO Time.getPOSIXTime

  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr



      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh ((U.posixToMilliseconds startTime) + 20))
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 
                                            ((U.posixToMilliseconds startTime) + 20) []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  time <- Q.waitForPOSIXTime era localNodeConnectInfo "Waiting for POSIX" (startTime + 31)
  slotNo <- Q.getCurrentSlotNo era localNodeConnectInfo
  
  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn3 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Cancel)
                 sbe Nothing Nothing exUnits)

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 
                                            ((U.posixToMilliseconds startTime) + 20) []) 
                                            , tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn3] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        , C.txValidityLowerBound = Tx.txValidityLowerBound era (slotNo - 100)
        , C.txValidityUpperBound = Tx.txValidityUpperBound era (slotNo + 200)
          -- \^ ~9min range (200ms slots)
          -- \^ Babbage era onwards cannot have upper slot beyond epoch boundary (10_000 slot epoch)
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w2VKey]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent3
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

proposeInCollectingFailTestInfo =
  TestInfo
    { testName = "proposeInCollectingFailTest"
    , testDescription =
        "Can't Propose in Collecting state"
    , test = proposeInCollectingFailTest
    }


proposeInCollectingFailTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
proposeInCollectingFailTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr


  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn2 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn2] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        }

  eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent3
        w2Addr
        Nothing
        [C.WitnessPaymentKey w2SKey]
  H.annotate $ show eitherTx
  let expError = "failed Validation"
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx



doublePayTestInfo =
  TestInfo
    { testName = "doublePayTest"
    , testDescription =
        "General correct example where we Propose, Add, Add, Pay, Propose, Add, Add, Pay "
    , test = doublePayTest
    }

doublePayTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
doublePayTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  -- (w4SKey, w4VKey, w4Addr) <- TN.w4All tempAbsPath networkId
  -- (w5SKey, w5VKey, w5Addr) <- TN.w5All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr
  --txIn' <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr
  --txIn'' <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w3Addr

--  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w3Addr



  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))
  let par = Params{authSigs = [w2Pkh, w3Pkh, w1Pkh], nr = 2}

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.smSpendScriptHashV2 par)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)
      tokenValue = C.valueFromList [((SM.ttAssetIdV2 plutusAddress (fromCardanoTxIn txIn)), 1)]
      mintWitness = Map.fromList [SM.ttMintWitness plutusAddress (fromCardanoTxIn txIn) sbe exUnits]
      tt = (P.AssetClass (PS.fromPolicyId (ttPolicyIdV2 plutusAddress (fromCardanoTxIn txIn)),"ThreadToken"))
      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , tToken = tt }))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

--    scriptTxOut2 = Tx.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000 <> tokenValue)
--                   scriptAddress (PS.toScriptData (Collecting (lovelaceValue 4200000) w1Pkh 1000 []))
    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn3 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn3] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w2VKey]
        }

  signedTx3 <- Tx.buildTx era localNodeConnectInfo txBodyContent3 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx3
  let txInAtScript3 = Tx.txIn (Tx.txId signedTx3) 0
  
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "waitForTxInAtAddress"


  txIn4 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w3Addr

  let
    collateral3 = Tx.txInsCollateral era [otherTxIn3] --Tx.txInsCollateral era [txIn4]
    -- without reference script
    scriptTxIn4 = Tx.txInWitness txInAtScript3 (SM.smSpendWitness par
                 (Add w3Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut4 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh, w3Pkh]) , 
            tToken = tt }))

    txBodyContent4 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn4] ++ (Tx.pubkeyTxIns [txIn4])
        , C.txInsCollateral = collateral3
        , C.txOuts = [scriptTxOut4]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w3VKey]
        }

  signedTx4 <- Tx.buildTx era localNodeConnectInfo txBodyContent4 w3Addr w3SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx4
  let txInAtScript4 = Tx.txIn (Tx.txId signedTx4) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript4 "waitForTxInAtAddress"
  
  txIn5 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Addr

  let
    -- without reference script
    scriptTxIn5 = Tx.txInWitness txInAtScript4 (SM.smSpendWitness par
                 (Pay)
                 sbe Nothing Nothing exUnits)

    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut5 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 5_800_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , 
            tToken = tt }))
    paymentTxOut = Tx.txOut era (C.lovelaceToValue 4_200_000) w1Addr

    txBodyContent5 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn5] ++ (Tx.pubkeyTxIns [txIn5])
        , C.txInsCollateral = collateral1
        , C.txOuts = [scriptTxOut5, paymentTxOut]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
        }

  signedTx5 <- Tx.buildTx era localNodeConnectInfo txBodyContent5 w1Addr w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx5
  let txInAtScript5 = Tx.txIn (Tx.txId signedTx5) 0
      payment = Tx.txIn (Tx.txId signedTx5) 1

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript5 "waitForTxInAtAddress"

  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo scriptAddress txInAtScript5 "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 5_800_000 <> tokenValue)
  assert "txOut has tokens" txOutHasAdaValue
  paymentTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Addr payment "getPayment"
  paymentHasAda <- Q.txOutHasValue paymentTxOut (C.lovelaceToValue 4_200_000 )
  assert "payment" paymentHasAda 

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript5 (SM.smSpendWitness par
                 (Propose (lovelaceValue 4200000) w1Pkh 1000)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 5_800_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 []) , 
            tToken = tt }))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn3 = Tx.txInWitness txInAtScript2 (SM.smSpendWitness par
                 (Add w2Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 5_800_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh]) , 
            tToken = tt }))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn3] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut3]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w2VKey]
        }

  signedTx3 <- Tx.buildTx era localNodeConnectInfo txBodyContent3 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx3
  let txInAtScript3 = Tx.txIn (Tx.txId signedTx3) 0
  
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "waitForTxInAtAddress"


  txIn4 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w3Addr

  let
    collateral3 = Tx.txInsCollateral era [otherTxIn3] --Tx.txInsCollateral era [txIn4]
    -- without reference script
    scriptTxIn4 = Tx.txInWitness txInAtScript3 (SM.smSpendWitness par
                 (Add w3Pkh)
                 sbe Nothing Nothing exUnits)

    scriptTxOut4 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 5_800_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = (Collecting (lovelaceValue 4200000) w1Pkh 1000 [w2Pkh, w3Pkh]) , 
            tToken = tt }))

    txBodyContent4 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn4] ++ (Tx.pubkeyTxIns [txIn4])
        , C.txInsCollateral = collateral3
        , C.txOuts = [scriptTxOut4]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w3VKey]
        }

  signedTx4 <- Tx.buildTx era localNodeConnectInfo txBodyContent4 w3Addr w3SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx4
  let txInAtScript4 = Tx.txIn (Tx.txId signedTx4) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript4 "waitForTxInAtAddress"
  
  txIn5 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Addr

  let
    -- without reference script
    scriptTxIn5 = Tx.txInWitness txInAtScript4 (SM.smSpendWitness par
                 (Pay)
                 sbe Nothing Nothing exUnits)

    collateral1 = Tx.txInsCollateral era [otherTxIn1]

    scriptTxOut5 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 1_600_000 <> tokenValue)
          scriptAddress
          (PS.toScriptData (State { label = Holding , 
            tToken = tt }))
    paymentTxOut = Tx.txOut era (C.lovelaceToValue 4_200_000) w1Addr

    txBodyContent5 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn5] ++ (Tx.pubkeyTxIns [txIn5])
        , C.txInsCollateral = collateral1
        , C.txOuts = [scriptTxOut5, paymentTxOut]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
        }

  signedTx5' <- Tx.buildTx era localNodeConnectInfo txBodyContent5 w1Addr w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx5'
  let txInAtScript5 = Tx.txIn (Tx.txId signedTx5') 0
      payment2 = Tx.txIn (Tx.txId signedTx5') 1

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript5 "waitForTxInAtAddress"

  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo scriptAddress txInAtScript5 "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 1_600_000 <> tokenValue)
  assert "txOut has tokens" txOutHasAdaValue
  paymentTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Addr payment2 "getPayment"
  paymentHasAda <- Q.txOutHasValue paymentTxOut (C.lovelaceToValue 4_200_000 )
  assert "payment" paymentHasAda


memTestInfo =
  TestInfo
    { testName = "memTest"
    , testDescription =
        "Force Memory Error"
    , test = memTest
    }

memTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
memTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do

  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  let (w1Pkh, w2Pkh, w3Pkh) = (waddrToPkh w1Addr, waddrToPkh w2Addr, waddrToPkh w3Addr)
        where
          waddrToPkh waddr = fromJust (P.toPubKeyHash (TC.toPlutusAddress (shelleyAddressInEra sbe waddr)))

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.mSpendScriptHashV2)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)

      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000)
          scriptAddress
          (PS.toScriptData () )
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    scriptTxIn = Tx.txInWitness txInAtScript (SM.mSpendWitness
                 [w1Pkh, w2Pkh, w3Pkh]
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 5_000_000)
          scriptAddress
          (PS.toScriptData ())

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 5_000_000 )
  assert "txOut has tokens" txOutHasAdaValue


countTestInfo =
  TestInfo
    { testName = "countTest"
    , testDescription =
        "Spend funds locked by script by using inline datum and providing Plutus script in "
          ++ "transaction as witness but simpler"
    , test = countTest
    }

countTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
countTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do

  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1VKey, w1Addr) <- TN.w1All tempAbsPath networkId
  (w2SKey, w2VKey, w2Addr) <- TN.w2All tempAbsPath networkId
  (w3SKey, w3VKey, w3Addr) <- TN.w3All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let scriptAddress = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra -> makeAddress (Right (SM.lcSpendScriptHashV2)) networkId
        C.ConwayEra -> error "Conway era is unsupported in this test"

  let exUnits = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 100_000_000}
      plutusAddress = TC.toPlutusAddress (shelleyAddressInEra sbe scriptAddress)

      collateral = Tx.txInsCollateral era [txIn]
      scriptTxOut =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 10_000_000)
          scriptAddress
          (PS.toScriptData ([1::Integer]))
      otherTxOut1 = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Addr
      otherTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Addr
      otherTxOut3 = Tx.txOut era (C.lovelaceToValue 5_000_000) w3Addr

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          --, C.txMintValue = Tx.txMintValue era tokenValue mintWitness
          , C.txOuts = [scriptTxOut, otherTxOut1, otherTxOut2, otherTxOut3]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn1 = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn2 = Tx.txIn (Tx.txId signedTx) 2
      otherTxIn3 = Tx.txIn (Tx.txId signedTx) 3
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

-- build a transaction to mint token using reference script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Addr

  let
    -- without reference script
    scriptTxIn = Tx.txInWitness txInAtScript (SM.lcSpendWitness
                 (2 :: Integer)
                 sbe Nothing Nothing exUnits)
    collateral2 = Tx.txInsCollateral era [otherTxIn2]

--    scriptTxOut2 = Tx.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000 <> tokenValue)
--                   scriptAddress (PS.toScriptData (Collecting (lovelaceValue 4200000) w1Pkh 1000 []))
    scriptTxOut2 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 9_000_000)
          scriptAddress
          (PS.toScriptData ([1::Integer, 2::Integer]))

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn] ++ (Tx.pubkeyTxIns [txIn2])
        , C.txInsCollateral = collateral2
        , C.txOuts = [scriptTxOut2]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let txInAtScript2 = Tx.txIn (Tx.txId signedTx2) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript2 "waitForTxInAtAddress"

  txIn3 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Addr

  let
    -- without reference script
    scriptTxIn3 = Tx.txInWitness txInAtScript2 (SM.lcSpendWitness
                 (6::Integer)
                 sbe Nothing Nothing exUnits)
    
    collateral3 = Tx.txInsCollateral era [otherTxIn1] --Tx.txInsCollateral era [txIn4]

    scriptTxOut3 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 8_000_000)
          scriptAddress
          (PS.toScriptData ([1::Integer,2::Integer,6::Integer]))

    txBodyContent3 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn3] ++ (Tx.pubkeyTxIns [txIn3])
        , C.txInsCollateral = collateral3
        , C.txOuts = [scriptTxOut3]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
        }

  signedTx3 <- Tx.buildTx era localNodeConnectInfo txBodyContent3 w1Addr w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx3
  let txInAtScript3 = Tx.txIn (Tx.txId signedTx3) 0
  
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "waitForTxInAtAddress"


  txIn4 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w3Addr

  let
    collateral4 = Tx.txInsCollateral era [otherTxIn3] --Tx.txInsCollateral era [txIn4]
    -- without reference script
    scriptTxIn4 = Tx.txInWitness txInAtScript3 (SM.lcSpendWitness
                 (100::Integer)
                 sbe Nothing Nothing exUnits)

    scriptTxOut4 =
        Tx.txOutWithInlineDatum
          era
          (C.lovelaceToValue 5_000_000)
          scriptAddress
          (PS.toScriptData ([1::Integer,2::Integer,6::Integer,100::Integer]))
    


    txBodyContent4 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn4] ++ (Tx.pubkeyTxIns [txIn4])
        , C.txInsCollateral = collateral4
        , C.txOuts = [scriptTxOut4]
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w3VKey]
        }

  signedTx4 <- Tx.buildTx era localNodeConnectInfo txBodyContent4 w3Addr w3SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx4
  let txInAtScript4 = Tx.txIn (Tx.txId signedTx4) 0

  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo scriptAddress txInAtScript4 "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 5_000_000 )
  assert "txOut has tokens" txOutHasAdaValue



