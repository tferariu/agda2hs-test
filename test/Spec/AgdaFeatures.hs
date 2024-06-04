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
        , C.txValidityLowerBound = Tx.txValidityLowerBound era 150
        , C.txValidityUpperBound = Tx.txValidityUpperBound era 300
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
  time <- Q.waitForPOSIXTime era localNodeConnectInfo "Waiting for POSIX" (startTime + 31)

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
        , C.txValidityLowerBound = Tx.txValidityLowerBound era 800
        , C.txValidityUpperBound = Tx.txValidityUpperBound era 1200
          -- \^ ~9min range (200ms slots)
          -- \^ Babbage era onwards cannot have upper slot beyond epoch boundary (10_000 slot epoch)
        , C.txExtraKeyWits = Tx.txExtraKeyWits era [w2VKey]
        }

  signedTx3 <- Tx.buildTx era localNodeConnectInfo txBodyContent3 w2Addr w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx3
  let txInAtScript3 = Tx.txIn (Tx.txId signedTx3) 0

  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "waitForTxInAtAddress"

  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo scriptAddress txInAtScript3 "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 10_000_000 <> tokenValue)
  assert "txOut has tokens" txOutHasAdaValue


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
  let expError = "failed Validation" -- fix
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx


addResultsInHoldingFailTestInfo =
  TestInfo
    { testName = "addResultsInHoldingFailTest"
    , testDescription =
        "Can't Add and end up in Holding state"
    , test = addUnsignedSignatureFailTest
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
  let expError = "failed Validation" -- fix
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx

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


smTestInfo =
  TestInfo
    { testName = "smTest"
    , testDescription =
        "Spend funds locked by script by using inline datum and providing Plutus script in "
          ++ "transaction as witness"
    , test = smTest
    }

smTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
smTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
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



inlineDatumSpendTestInfo =
  TestInfo
    { testName = "inlineDatumSpendTest"
    , testDescription =
        "Spend funds locked by script by using inline datum and providing Plutus script in "
          ++ "transaction as witness"
    , test = inlineDatumSpendTest
    }

inlineDatumSpendTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
inlineDatumSpendTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let scriptAddress = case era of
        C.BabbageEra -> makeAddress (Right PS_1_0.alwaysSucceedSpendScriptHashV2) networkId
        C.ConwayEra -> makeAddress (Right PS_1_1.alwaysSucceedSpendScriptHashV3) networkId
      scriptTxOut = Tx.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000) scriptAddress (PS.toScriptData ())
      otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txOuts = [scriptTxOut, otherTxOut]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn = Tx.txIn (Tx.txId signedTx) 1
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let
    -- without reference script
    scriptTxIn = case era of
      C.BabbageEra -> Tx.txInWitness txInAtScript (PS_1_0.alwaysSucceedSpendWitnessV2 sbe Nothing Nothing)
      C.ConwayEra -> Tx.txInWitness txInAtScript (PS_1_1.alwaysSucceedSpendWitnessV3 sbe Nothing Nothing)
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn]
        , C.txInsCollateral = collateral
        , C.txOuts = [txOut]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  assert "txOut has tokens" txOutHasAdaValue

checkTxInfoV2TestInfo =
  TestInfo
    { testName = "checkTxInfoV2Test"
    , testDescription =
        "Check each attribute of the TxInfo from the V2 ScriptContext in a single transaction"
    , test = checkTxInfoV2Test
    }

checkTxInfoV2Test
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
checkTxInfoV2Test networkOptions TestParams{..} = do
  era <- TN.eraFromOptionsM networkOptions
  startTime <- liftIO Time.getPOSIXTime
  (w1SKey, w1VKey, w1Address) <- TN.w1All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  txInAsTxOut@(C.TxOut _ txInValue _ _) <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "txInAsTxOut <- getTxOutAtAddress"

  let tokenValues = C.valueFromList [(PS.checkV2TxInfoAssetIdV2, 1), (PS_1_0.alwaysSucceedAssetIdV2, 2)]
      executionUnits1 = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 10_000_000}
      executionUnits2 = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 4_000_000}
      collateral = Tx.txInsCollateral era [txIn]
      totalLovelace = C.txOutValueToLovelace txInValue
      fee = 2_500_000 :: C.Lovelace
      amountPaid = 10_000_000
      amountReturned = totalLovelace - amountPaid - fee
      datum = PS.toScriptData (42 :: Integer)

      txOut1 = Tx.txOutWithDatumInTx era (C.lovelaceToValue amountPaid <> tokenValues) w1Address datum
      txOut2 = Tx.txOut era (C.lovelaceToValue amountReturned) w1Address

      lowerBound =
        P.fromMilliSeconds $
          P.DiffMilliSeconds $
            U.posixToMilliseconds $
              Time.utcTimeToPOSIXSeconds $
                -- subtract 10 seconds from the lower bound so it is well before the testnet start time
                Time.addUTCTime (-10) $
                  fromJust mTime -- before slot 1
      upperBound =
        P.fromMilliSeconds $
          P.DiffMilliSeconds $
            U.posixToMilliseconds startTime
              + 600_000 -- ~10mins after slot 1 (to account for testnet init time)
      timeRange = P.interval lowerBound upperBound :: P.POSIXTimeRange

      expTxInfoInputs = PS.txInfoInputs era (txIn, txInAsTxOut)
      expTxInfoReferenceInputs = PS.txInfoInputs era (txIn, txInAsTxOut)
      expTxInfoOutputs = PS.txInfoOutputs era [txOut1, txOut2]
      expTxInfoFee = PS.txInfoFee fee
      expTxInfoMint = PS.txInfoMint tokenValues
      expDCert = [] -- not testing any staking registration certificate
      expWdrl = PV2.fromList [] -- not testing any staking reward withdrawal
      expTxInfoSigs = PS.txInfoSigs [w1VKey]
      expTxInfoRedeemers = PS_1_0.alwaysSucceedPolicyTxInfoRedeemerV2
      expTxInfoData = PS.txInfoData [datum]
      expTxInfoValidRange = timeRange

      redeemer =
        PS.checkV2TxInfoRedeemer
          [expTxInfoInputs]
          [expTxInfoReferenceInputs]
          expTxInfoOutputs
          expTxInfoFee
          expTxInfoMint
          expDCert
          expWdrl
          expTxInfoValidRange
          expTxInfoSigs
          expTxInfoRedeemers
          expTxInfoData
      mintWitnesses =
        Map.fromList
          [ PS.checkV2TxInfoMintWitnessV2 sbe redeemer executionUnits1
          , PS_1_0.alwaysSucceedMintWitnessV2' sbe executionUnits2
          ]

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsReference = Tx.txInsReference era [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
          , C.txOuts = [txOut1, txOut2]
          , C.txFee = Tx.txFee era fee
          , C.txValidityLowerBound = Tx.txValidityLowerBound era 1
          , C.txValidityUpperBound = Tx.txValidityUpperBound era 2700 
          , -- \^ ~9min range (200ms slots)
            -- \^ Babbage era onwards cannot have upper slot beyond epoch boundary (10_000 slot epoch)
            C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
          }
  txbody <- Tx.buildRawTx sbe txBodyContent
  kw <- Tx.signTx sbe txbody (C.WitnessPaymentKey w1SKey)
  let signedTx = C.makeSignedTransaction [kw] txbody

  Tx.submitTx sbe localNodeConnectInfo signedTx

  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0
  resultTxOut <-
    Q.getTxOutAtAddress
      era
      localNodeConnectInfo
      w1Address
      expectedTxIn
      "resultTxOut <- getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  assert "txOut has tokens" txOutHasTokenValue

referenceScriptMintTestInfo =
  TestInfo
    { testName = "referenceScriptMintTest"
    , testDescription = "Mint tokens by referencing an input containing a Plutus policy as witness"
    , test = referenceScriptMintTest
    }

referenceScriptMintTest
  :: (MonadTest m, MonadIO m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
referenceScriptMintTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let refScriptLovelaceValue = C.lovelaceToValue 20_000_000
      refScriptTxOut = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra ->
          Tx.txOutWithRefScript
            era
            refScriptLovelaceValue
            w1Address
            (PS.unPlutusScriptV2 PS_1_0.alwaysSucceedPolicyScriptV2)
        C.ConwayEra ->
          Tx.txOutWithRefScript
            era
            refScriptLovelaceValue
            w1Address
            (PS.unPlutusScriptV3 PS_1_1.alwaysSucceedPolicyScriptV3)
      otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txOuts = [refScriptTxOut, otherTxOut]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn = Tx.txIn (Tx.txId signedTx) 1
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let (tokenValues, mintWitnesses) = case era of
        C.BabbageEra ->
          ( C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 6)]
          , Map.fromList [PS_1_0.alwaysSucceedMintWitnessV2 sbe (Just refScriptTxIn)]
          )
        C.ConwayEra ->
          ( C.valueFromList [(PS_1_1.alwaysSucceedAssetIdV3, 6)]
          , Map.fromList [PS_1_1.alwaysSucceedMintWitnessV3 sbe (Just refScriptTxIn)]
          )
      collateral = Tx.txInsCollateral era [otherTxIn]
      txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

      txBodyContent2 =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [otherTxIn]
          , C.txInsCollateral = collateral
          , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
          , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
          , C.txOuts = [txOut]
          }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  assert "txOut has tokens" txOutHasTokenValue

referenceScriptInlineDatumSpendTestInfo =
  TestInfo
    { testName = "referenceScriptInlineDatumSpendTest"
    , testDescription =
        "Spend funds locked by script by using inline datum and referencing an input containing a "
          ++ "Plutus script as witness"
    , test = referenceScriptInlineDatumSpendTest
    }

referenceScriptInlineDatumSpendTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
referenceScriptInlineDatumSpendTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    -- build a transaction to hold reference script

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    let refScriptLovelaceValue = C.lovelaceToValue 20_000_000
        refScriptTxOut = case era of
          C.AlonzoEra -> error "Alonzo era is unsupported in this test"
          C.BabbageEra ->
            Tx.txOutWithRefScript
              era
              refScriptLovelaceValue
              w1Address
              (PS.unPlutusScriptV2 PS_1_0.alwaysSucceedSpendScriptV2)
          C.ConwayEra ->
            Tx.txOutWithRefScript
              era
              refScriptLovelaceValue
              w1Address
              (PS.unPlutusScriptV3 PS_1_1.alwaysSucceedSpendScriptV3)
        otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address
        scriptAddress = case era of
          C.BabbageEra -> makeAddress (Right PS_1_0.alwaysSucceedSpendScriptHashV2) networkId
          C.ConwayEra -> makeAddress (Right PS_1_1.alwaysSucceedSpendScriptHashV3) networkId
        scriptTxOut = Tx.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000) scriptAddress (PS.toScriptData ())
        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txOuts = [refScriptTxOut, otherTxOut, scriptTxOut]
            }

    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
        otherTxIn = Tx.txIn (Tx.txId signedTx) 1
        txInAtScript = Tx.txIn (Tx.txId signedTx) 2
    Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "waitForTxInAtAddress"

    -- build a transaction to mint token using reference script

    let witness = case era of
          C.BabbageEra -> PS_1_0.alwaysSucceedSpendWitnessV2 sbe (Just refScriptTxIn) Nothing
          C.ConwayEra -> PS_1_1.alwaysSucceedSpendWitnessV3 sbe (Just refScriptTxIn) Nothing
        scriptTxIn = Tx.txInWitness txInAtScript witness
        collateral = Tx.txInsCollateral era [otherTxIn]
        adaValue = C.lovelaceToValue 4_200_000
        txOut = Tx.txOut era adaValue w1Address

        txBodyContent2 =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = [scriptTxIn]
            , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
            , C.txInsCollateral = collateral
            , C.txOuts = [txOut]
            }

    signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
    -- Query for txo and assert it contains newly minted token
    resultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
    txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
    assert "txOut has tokens" txOutHasAdaValue

referenceScriptDatumHashSpendTestInfo =
  TestInfo
    { testName = "referenceScriptDatumHashSpendTest"
    , testDescription =
        "Spend funds locked by script by providing datum in txbody and referencing an input "
          ++ "containing a Plutus script as witness"
    , test = referenceScriptDatumHashSpendTest
    }

referenceScriptDatumHashSpendTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
referenceScriptDatumHashSpendTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let refScriptLovelaceValue = C.lovelaceToValue 20_000_000
      refScriptTxOut = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra ->
          Tx.txOutWithRefScript
            era
            refScriptLovelaceValue
            w1Address
            (PS.unPlutusScriptV2 PS_1_0.alwaysSucceedSpendScriptV2)
        C.ConwayEra ->
          Tx.txOutWithRefScript
            era
            refScriptLovelaceValue
            w1Address
            (PS.unPlutusScriptV3 PS_1_1.alwaysSucceedSpendScriptV3)
      otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address
      scriptAddress = case era of
        C.BabbageEra -> makeAddress (Right PS_1_0.alwaysSucceedSpendScriptHashV2) networkId
        C.ConwayEra -> makeAddress (Right PS_1_1.alwaysSucceedSpendScriptHashV3) networkId
      datum = PS.toScriptData ()
      scriptTxOut = Tx.txOutWithDatumHash era (C.lovelaceToValue 10_000_000) scriptAddress datum

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txOuts = [refScriptTxOut, otherTxOut, scriptTxOut]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn = Tx.txIn (Tx.txId signedTx) 1
      txInAtScript = Tx.txIn (Tx.txId signedTx) 2
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let scriptTxIn = case era of
        C.BabbageEra ->
          Tx.txInWitness
            txInAtScript
            (PS_1_0.alwaysSucceedSpendWitnessV2 sbe (Just refScriptTxIn) (Just datum))
        C.ConwayEra ->
          Tx.txInWitness
            txInAtScript
            (PS_1_1.alwaysSucceedSpendWitnessV3 sbe (Just refScriptTxIn) (Just datum))
      collateral = Tx.txInsCollateral era [otherTxIn]
      adaValue = C.lovelaceToValue 4_200_000
      txOut = Tx.txOut era adaValue w1Address

      txBodyContent2 =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = [scriptTxIn]
          , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
          , C.txInsCollateral = collateral
          , C.txOuts = [txOut]
          }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  assert "txOut has tokens" txOutHasAdaValue

referenceInputWithV1ScriptErrorTestInfo =
  TestInfo
    { testName = "referenceInputWithV1ScriptErrorTest"
    , testDescription =
        "ReferenceInputsNotSupported error occurs when executing a V1 script whilst referencing an input"
    , test = referenceInputWithV1ScriptErrorTest
    }

referenceInputWithV1ScriptErrorTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
referenceInputWithV1ScriptErrorTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    let tokenValues = C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV1, 1)]
        mintWitnesses = Map.fromList [PS_1_0.alwaysSucceedMintWitnessV1 sbe Nothing]
        collateral = Tx.txInsCollateral era [txIn]
        txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txInsCollateral = collateral
            , C.txInsReference = Tx.txInsReference era [txIn]
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut]
            }

    eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent
        w1Address
        Nothing
        [C.WitnessPaymentKey w1SKey]
    let expError = "ReferenceInputsNotSupported"
    -- why is this validity interval error? https://github.com/input-output-hk/cardano-node/issues/5080
    assert expError $ Tx.isTxBodyErrorValidityInterval expError eitherTx

referenceScriptOutputWithV1ScriptErrorTestInfo =
  TestInfo
    { testName = "referenceScriptOutputWithV1ScriptErrorTest"
    , testDescription =
        "ReferenceScriptsNotSupported error occurs when executing a V1 script whilst creating "
          ++ "an output including a reference script"
    , test = referenceScriptOutputWithV1ScriptErrorTest
    }

referenceScriptOutputWithV1ScriptErrorTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
referenceScriptOutputWithV1ScriptErrorTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    let tokenValues = C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV1, 1)]
        mintWitnesses = Map.fromList [PS_1_0.alwaysSucceedMintWitnessV1 sbe Nothing]
        collateral = Tx.txInsCollateral era [txIn]
        txOut =
          Tx.txOutWithRefScript
            era
            (C.lovelaceToValue 3_000_000 <> tokenValues)
            w1Address
            (PS.unPlutusScriptV2 PS_1_0.alwaysSucceedSpendScriptV2)

        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txInsCollateral = collateral
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut]
            }

    eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent
        w1Address
        Nothing
        [C.WitnessPaymentKey w1SKey]
    H.annotate $ show eitherTx
    let expError = "ReferenceScriptsNotSupported"
    -- why is this validity interval error? https://github.com/input-output-hk/cardano-node/issues/5080
    assert expError $ Tx.isTxBodyErrorValidityInterval expError eitherTx

inlineDatumOutputWithV1ScriptErrorTestInfo =
  TestInfo
    { testName = "inlineDatumOutputWithV1ScriptErrorTest"
    , testDescription =
        "InlineDatumsNotSupported error occurs when executing a V1 script whilst creating"
          ++ "an output including an inline datum"
    , test = inlineDatumOutputWithV1ScriptErrorTest
    }

inlineDatumOutputWithV1ScriptErrorTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
inlineDatumOutputWithV1ScriptErrorTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    let tokenValues = C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV1, 1)]
        mintWitnesses = Map.fromList [PS_1_0.alwaysSucceedMintWitnessV1 sbe Nothing]
        collateral = Tx.txInsCollateral era [txIn]
        txOut =
          Tx.txOutWithInlineDatum
            era
            (C.lovelaceToValue 3_000_000 <> tokenValues)
            w1Address
            (PS.toScriptData ())

        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txInsCollateral = collateral
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut]
            }

    eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent
        w1Address
        Nothing
        [C.WitnessPaymentKey w1SKey]
    H.annotate $ show eitherTx
    let expError = "InlineDatumsNotSupported"
    -- why is this validity interval error? https://github.com/input-output-hk/cardano-node/issues/5080
    assert expError $ Tx.isTxBodyErrorValidityInterval expError eitherTx

returnCollateralWithTokensValidScriptTestInfo =
  TestInfo
    { testName = "returnCollateralWithTokensValidScriptTest"
    , testDescription =
        "Check it is possible to provide collateral input containing tokens if return collateral"
          ++ "is being used to return them"
    , test = returnCollateralWithTokensValidScriptTest
    }

returnCollateralWithTokensValidScriptTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
returnCollateralWithTokensValidScriptTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    -- build and submit transaction to create output containing some tokens.

    let (tokenValues, mintWitnesses) = case era of
          C.AlonzoEra -> error "Alonzo era is unsupported in this test"
          C.BabbageEra ->
            ( C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 10)]
            , Map.fromList [PS_1_0.alwaysSucceedMintWitnessV2 sbe Nothing]
            )
          C.ConwayEra ->
            ( C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 10), (PS_1_1.alwaysSucceedAssetIdV3, 10)]
            , Map.fromList
                [ PS_1_0.alwaysSucceedMintWitnessV2 sbe Nothing
                , PS_1_1.alwaysSucceedMintWitnessV3 sbe Nothing
                ]
            )
        collateral = Tx.txInsCollateral era [txIn]
        txOut =
          Tx.txOutWithInlineDatum
            era
            (C.lovelaceToValue 5_000_000 <> tokenValues)
            w1Address
            (PS.toScriptData ())

        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txInsCollateral = collateral
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut]
            }

    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let txIn2 = Tx.txIn (Tx.txId signedTx) 0
    Q.waitForTxInAtAddress era localNodeConnectInfo w1Address txIn2 "waitForTxInAtAddress"

    -- build and submit transaction with tokens in collateral input.
    -- This is allowed because using return collateral feature.

    let tokenValues2 = case era of
          C.BabbageEra -> C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 20)]
          C.ConwayEra -> C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 20), (PS_1_1.alwaysSucceedAssetIdV3, 20)]
        collateral2 = Tx.txInsCollateral era [txIn2]
        txOut2 =
          Tx.txOutWithInlineDatum
            era
            (C.lovelaceToValue 2_000_000 <> tokenValues2)
            w1Address
            (PS.toScriptData ())
        colReturnTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

        txBodyContent2 =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn2]
            , C.txInsCollateral = collateral2
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut2]
            , C.txReturnCollateral = Tx.txReturnCollateral era colReturnTxOut
            }

    signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
    -- Query for txo and assert it contains newly minted token
    resultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
    txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues2
    assert "txOut has tokens" txOutHasTokenValue

submitWithInvalidScriptThenCollateralIsTakenAndReturnedTestInfo =
  TestInfo
    { testName = "submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest"
    , testDescription =
        "Submit a failing script when using total and return collateral in tx body."
          ++ "Check that ada and tokens from collateral input are returned in the collateral output."
          ++ "and that the regular input is not consumed."
    , test = submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest
    }
submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    -- build and submit transaction to create output containing some tokens.

    let (tokenValues, mintWitnesses) = case era of
          C.AlonzoEra -> error "Alonzo era is unsupported in this test"
          C.BabbageEra ->
            ( C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 10)]
            , Map.fromList [PS_1_0.alwaysSucceedMintWitnessV2 sbe Nothing]
            )
          C.ConwayEra ->
            ( C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 10), (PS_1_1.alwaysSucceedAssetIdV3, 10)]
            , Map.fromList
                [PS_1_0.alwaysSucceedMintWitnessV2 sbe Nothing, PS_1_1.alwaysSucceedMintWitnessV3 sbe Nothing]
            )
        collateral = Tx.txInsCollateral era [txIn]
        txOutAmount = 10_000_000
        txOut = Tx.txOut era (C.lovelaceToValue txOutAmount <> tokenValues) w1Address

        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txInsCollateral = collateral
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut]
            }

    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let collateralTxIn = Tx.txIn (Tx.txId signedTx) 0
    Q.waitForTxInAtAddress era localNodeConnectInfo w1Address collateralTxIn "waitForTxInAtAddress"

    -- build and submit transaction with failing script

    txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    let (tokenValues2, mintWitnesses2) = case era of
          C.BabbageEra ->
            ( C.valueFromList [(PS_1_0.alwaysFailsAssetIdV2, 1)]
            , Map.fromList [PS_1_0.alwaysFailsMintWitnessV2 sbe Nothing]
            )
          C.ConwayEra ->
            ( C.valueFromList [(PS_1_1.alwaysFailsAssetIdV3, 1)]
            , Map.fromList [PS_1_1.alwaysSucceedMintWitnessV3 sbe Nothing]
            )
        collateral2 = Tx.txInsCollateral era [collateralTxIn]
        txOut1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
        txOut2 = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues2) w1Address
        colReturnAmount = 4_000_000
        colReturnValue = C.lovelaceToValue colReturnAmount <> tokenValues
        colReturnTxOut = Tx.txOut era colReturnValue w1Address
        totalCollateralAmount = txOutAmount - colReturnAmount

        txBodyContent2 =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn2]
            , C.txInsCollateral = collateral2
            , C.txMintValue = Tx.txMintValue era tokenValues2 mintWitnesses2
            , C.txOuts = [txOut1, txOut2]
            , C.txReturnCollateral = Tx.txReturnCollateral era colReturnTxOut
            , C.txTotalCollateral = Tx.txTotalCollateral era totalCollateralAmount
            , C.txScriptValidity = Tx.txScriptValidity era C.ScriptInvalid
            }

    signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx2

    -- Query for return collateral txo and assert presence of ada and tokens from the first tx
    let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 3 -- collateral return index is n outputs (including change)
    resultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
    txOutHasAdaAndTokenValue <- Q.txOutHasValue resultTxOut colReturnValue
    a1 <- assert "txOut has tokens" txOutHasAdaAndTokenValue
    -- Query collateral input and assert it has been spent
    collateralSpent <- not <$> Q.isTxOutAtAddress era localNodeConnectInfo w1Address collateralTxIn
    a2 <- assert "collateral spent" collateralSpent
    -- Query regular tx input and assert it has not been spent
    txInNotSpent <- Q.isTxOutAtAddress era localNodeConnectInfo w1Address txIn2
    a3 <- assert "txIn not spent" txInNotSpent
    U.concatMaybes [a1, a2, a3]

-- TODO: access datum in reference input in plutus script
