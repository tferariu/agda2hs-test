{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}
module PlutusScripts.Agda.SM where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1 (Redeemer, ScriptPurpose (Minting))
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusScripts.Basic.Common (
  mkAlwaysFailsPolicy,
  mkAlwaysSucceedPolicy,
  mkAlwaysSucceedSpend,
 )
import PlutusScripts.Helpers (
  asRedeemer,
  fromPolicyId,
  mintScriptWitness,
  mintScriptWitness',
  plutusL1,
  plutusL2,
  policyIdV1,
  policyIdV2,
  spendScriptWitness,
  toScriptData,
  unPlutusScriptV1,
  unPlutusScriptV2,
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AMap

import PlutusCore.Core qualified as PLC
import Helpers.ScriptUtils
import PlutusScripts.Agda.Common

--SM validator

{-
smValidator :: Params -> SerialisedScript
smValidator param =
  serialiseCompiledCode $
    $$(PlutusTx.compile [|| \a -> mkUntypedValidator @PlutusV2.ScriptContext (mkValidator a) ||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion100 param


-}
--TT policy
{-
ttPolicy :: PlutusV2.Address -> PlutusV2.TxOutRef -> PlutusV2.TokenName -> SerialisedScript
ttPolicy adr outref tn =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||\a b c -> mkUntypedMintingPolicy @PlutusV2.ScriptContext (mkPolicy a b c)||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion100 adr
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion100 outref
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion100 tn
-}
--mkPolicy :: Address -> TxOutRef -> TokenName ->  () -> ScriptContext -> Bool


-- AlwaysSucceeds minting policy --

alwaysSucceedPolicy :: SerialisedScript
alwaysSucceedPolicy = serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysSucceedPolicy||])

alwaysSucceedPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
alwaysSucceedPolicyScriptV1 = C.PlutusScriptSerialised alwaysSucceedPolicy

alwaysSucceedPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysSucceedPolicyScriptV2 = C.PlutusScriptSerialised alwaysSucceedPolicy

alwaysSucceedPolicyIdV2 :: C.PolicyId
alwaysSucceedPolicyIdV2 = policyIdV2 alwaysSucceedPolicy

alwaysSucceedPolicyScriptHashV1 :: C.ScriptHash
alwaysSucceedPolicyScriptHashV1 = C.hashScript $ unPlutusScriptV1 alwaysSucceedPolicyScriptV1

alwaysSucceedPolicyScriptHashV2 :: C.ScriptHash
alwaysSucceedPolicyScriptHashV2 = C.hashScript $ unPlutusScriptV2 alwaysSucceedPolicyScriptV2

alwaysSucceedAssetIdV1 :: C.AssetId
alwaysSucceedAssetIdV1 = C.AssetId (policyIdV1 alwaysSucceedPolicy) ""

alwaysSucceedAssetIdV2 :: C.AssetId
alwaysSucceedAssetIdV2 = C.AssetId alwaysSucceedPolicyIdV2 ""

alwaysSucceedPolicyTxInfoRedeemerV2 :: PlutusV2.Map ScriptPurpose Redeemer
alwaysSucceedPolicyTxInfoRedeemerV2 =
  AMap.singleton
    (Minting $ fromPolicyId alwaysSucceedPolicyIdV2)
    (asRedeemer $ PlutusTx.toBuiltinData ())

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysSucceedMintWitnessV1
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV1 era Nothing =
  ( policyIdV1 alwaysSucceedPolicy
  , mintScriptWitness era plutusL1 (Left alwaysSucceedPolicyScriptV1) (toScriptData ())
  )
alwaysSucceedMintWitnessV1 era (Just refTxIn) =
  ( policyIdV1 alwaysSucceedPolicy
  , mintScriptWitness era plutusL1 (Right refTxIn) (toScriptData ())
  )

alwaysSucceedMintWitnessV1'
  :: C.ShelleyBasedEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV1' era exunits =
  ( policyIdV1 alwaysSucceedPolicy
  , mintScriptWitness' era plutusL1 (Left alwaysSucceedPolicyScriptV1) (toScriptData ()) exunits
  )

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysSucceedMintWitnessV2
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV2 era Nothing =
  ( policyIdV2 alwaysSucceedPolicy
  , mintScriptWitness era plutusL2 (Left alwaysSucceedPolicyScriptV2) (toScriptData ())
  )
alwaysSucceedMintWitnessV2 era (Just refTxIn) =
  ( policyIdV2 alwaysSucceedPolicy
  , mintScriptWitness era plutusL2 (Right refTxIn) (toScriptData ())
  )

alwaysSucceedMintWitnessV2'
  :: C.ShelleyBasedEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV2' era exunits =
  ( policyIdV2 alwaysSucceedPolicy
  , mintScriptWitness' era plutusL2 (Left alwaysSucceedPolicyScriptV2) (toScriptData ()) exunits
  )

-- AlwaysSucceeds validator --

alwaysSucceedSpend :: SerialisedScript
alwaysSucceedSpend = serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysSucceedSpend||])

alwaysSucceedSpendScriptV1 :: C.PlutusScript C.PlutusScriptV1
alwaysSucceedSpendScriptV1 = C.PlutusScriptSerialised alwaysSucceedSpend

alwaysSucceedSpendScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysSucceedSpendScriptV2 = C.PlutusScriptSerialised alwaysSucceedSpend

alwaysSucceedSpendScriptHashV1 :: C.ScriptHash
alwaysSucceedSpendScriptHashV1 = C.hashScript $ unPlutusScriptV1 alwaysSucceedSpendScriptV1

alwaysSucceedSpendScriptHashV2 :: C.ScriptHash
alwaysSucceedSpendScriptHashV2 = C.hashScript $ unPlutusScriptV2 alwaysSucceedSpendScriptV2

alwaysSucceedSpendWitnessV1
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn
  -> Maybe C.HashableScriptData
  -> C.Witness C.WitCtxTxIn era
alwaysSucceedSpendWitnessV1 era mRefScript mDatum =
  C.ScriptWitness C.ScriptWitnessForSpending $
    spendScriptWitness
      era
      plutusL1
      (maybe (Left alwaysSucceedSpendScriptV1) (\refScript -> Right refScript) mRefScript) -- script or reference script
      (maybe C.InlineScriptDatum (\datum -> C.ScriptDatumForTxIn datum) mDatum) -- inline datum or datum value
      (toScriptData ()) -- redeemer

alwaysSucceedSpendWitnessV2
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn
  -> Maybe C.HashableScriptData
  -> C.Witness C.WitCtxTxIn era
alwaysSucceedSpendWitnessV2 era mRefScript mDatum =
  C.ScriptWitness C.ScriptWitnessForSpending $
    spendScriptWitness
      era
      plutusL2
      (maybe (Left alwaysSucceedSpendScriptV2) (\refScript -> Right refScript) mRefScript) -- script or reference script
      (maybe C.InlineScriptDatum (\datum -> C.ScriptDatumForTxIn datum) mDatum) -- inline datum or datum value
      (toScriptData ()) -- redeemer

-- AlwaysFails minting policy --

alwaysFailsPolicy :: SerialisedScript
alwaysFailsPolicy = serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysFailsPolicy||])

alwaysFailsPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
alwaysFailsPolicyScriptV1 = C.PlutusScriptSerialised alwaysFailsPolicy

alwaysFailsPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysFailsPolicyScriptV2 = C.PlutusScriptSerialised alwaysFailsPolicy

alwaysFailsPolicyIdV2 :: C.PolicyId
alwaysFailsPolicyIdV2 = policyIdV2 alwaysFailsPolicy

alwaysFailsAssetIdV1 :: C.AssetId
alwaysFailsAssetIdV1 = C.AssetId (policyIdV1 alwaysFailsPolicy) ""

alwaysFailsAssetIdV2 :: C.AssetId
alwaysFailsAssetIdV2 = C.AssetId alwaysFailsPolicyIdV2 ""

alwaysFailsPolicyTxInfoRedeemerV2 :: PlutusV2.Map ScriptPurpose Redeemer
alwaysFailsPolicyTxInfoRedeemerV2 =
  AMap.singleton
    (Minting $ fromPolicyId alwaysFailsPolicyIdV2)
    (asRedeemer $ PlutusTx.toBuiltinData ())

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysFailsMintWitnessV1
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV1 era Nothing =
  ( policyIdV1 alwaysFailsPolicy
  , mintScriptWitness era plutusL1 (Left alwaysFailsPolicyScriptV1) (toScriptData ())
  )
alwaysFailsMintWitnessV1 era (Just refTxIn) =
  ( policyIdV1 alwaysFailsPolicy
  , mintScriptWitness era plutusL1 (Right refTxIn) (toScriptData ())
  )

alwaysFailsMintWitnessV1'
  :: C.ShelleyBasedEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV1' era exunits =
  ( policyIdV1 alwaysFailsPolicy
  , mintScriptWitness' era plutusL1 (Left alwaysFailsPolicyScriptV1) (toScriptData ()) exunits
  )

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysFailsMintWitnessV2
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV2 era Nothing =
  ( policyIdV2 alwaysFailsPolicy
  , mintScriptWitness era plutusL2 (Left alwaysFailsPolicyScriptV2) (toScriptData ())
  )
alwaysFailsMintWitnessV2 era (Just refTxIn) =
  ( policyIdV2 alwaysFailsPolicy
  , mintScriptWitness era plutusL2 (Right refTxIn) (toScriptData ())
  )

alwaysFailsMintWitnessV2'
  :: C.ShelleyBasedEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV2' sbe exunits =
  ( policyIdV2 alwaysFailsPolicy
  , mintScriptWitness' sbe plutusL2 (Left alwaysFailsPolicyScriptV2) (toScriptData ()) exunits
  )

{-
module PlutusScripts.Agda.SM (
  -- $escrow
  State (..),
  Params (..),
{--}
  smTypedValidator,
  mkAddress,
  insert,
{-
  -- * Actions
  propose,
  add,
  pay,
  cancel,
  start,
  TxSuccess (..),
  -}
  -- * Exposed for test endpoints
  Input (..),
  Datum,
  Deadline,
  Label (..),
  mkValidator,
  mkPolicy,
  policy,
  curSymbol,
  mintingHash,

  -- * Coverage
  covIdx, 
) where

import Control.Lens (makeClassyPrisms)
import Control.Monad (void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.RWS.Class (asks)
import Data.Map qualified as Map

import Data.Set qualified as Set
import PlutusCore qualified

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusTx (ToData)
import PlutusTx qualified
import PlutusTx.Code (getCovIdx)
import PlutusTx.Coverage (CoverageIndex, addCoverageMetadata, Metadata(..))
import PlutusTx.Prelude (traceError, traceIfFalse)
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Prelude --(Eq, Bool, Integer)
--import PlutusTx

import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.Internal.Node (
  SlotConfig,
  pSlotConfig,
  posixTimeRangeToContainedSlotRange,
 )
import Cardano.Node.Emulator.Test (testnet)
import Data.Maybe (fromJust)
import Ledger (POSIXTime, PaymentPubKeyHash (unPaymentPubKeyHash), TxId, getCardanoTxId)
import Ledger qualified
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts (validatorCardanoAddress)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.Scripts (ValidatorHash, datumHash)
import Plutus.Script.Utils.V2.Contexts (
  ScriptContext (ScriptContext, scriptContextTxInfo),
  TxInfo,
  scriptOutputsAt,
  txInfoValidRange,
  txSignedBy,
 )
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value --(Value, geq, lt)
import PlutusLedgerApi.V1.Interval qualified as Interval
--import PlutusLedgerApi.V1.Tx
import PlutusLedgerApi.V1.Address
import PlutusLedgerApi.V2 hiding (TxId)--(Datum (Datum))
import PlutusLedgerApi.V2.Contexts hiding (TxId)--(valuePaidTo)
import PlutusLedgerApi.V2.Tx hiding (TxId)--(OutputDatum (OutputDatum))

import           Prelude              (Show (..), String)

import PlutusCore.Version (plcVersion100)

minVal :: Integer --Lovelace
minVal = 2000000


type Deadline = Integer


data Label = Holding
           | Collecting Value PaymentPubKeyHash Deadline [PaymentPubKeyHash]
       deriving (Show)

{-# INLINABLE lEq #-}
lEq :: Label -> Label -> Bool
lEq Holding Holding = True
lEq Holding (Collecting _ _ _ _) = False
lEq (Collecting _ _ _ _) Holding = False
lEq (Collecting v pkh d sigs) (Collecting v' pkh' d' sigs') = v == v' && pkh == pkh' && d == d' && sigs == sigs'


instance Eq Label where
    {-# INLINABLE (==) #-}
    b == c = lEq b c


data State = State
    { label  :: Label
    , tToken :: AssetClass
    } deriving (Show)

instance Eq State where
    {-# INLINABLE (==) #-}
    b == c = (label b  == label c) &&
             (tToken b == tToken c)


data Input = Propose Value PaymentPubKeyHash Deadline
           | Add PaymentPubKeyHash
           | Pay
           | Cancel
          deriving (Show)


PlutusTx.unstableMakeIsData ''Label
PlutusTx.makeLift ''Label
PlutusTx.unstableMakeIsData ''Input
PlutusTx.makeLift ''Input
PlutusTx.unstableMakeIsData ''State
PlutusTx.makeLift ''State




{-# INLINABLE query #-}
query :: PaymentPubKeyHash -> [PaymentPubKeyHash] -> Bool
query pkh [] = False
query pkh (x : l') = x == pkh || (query pkh l')

{-# INLINABLE insert #-}
insert :: PaymentPubKeyHash -> [PaymentPubKeyHash] -> [PaymentPubKeyHash]
insert pkh [] = [pkh]
insert pkh (x : l')
  = if x == pkh then x : l' else x : insert pkh l'

{-# INLINABLE count #-}
count :: [PaymentPubKeyHash] -> Integer
count [] = 0
count (x : l) = 1 + count l

data Params = Params {authSigs :: [PaymentPubKeyHash], nr :: Integer}
    deriving (Show)

PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params




{-# INLINABLE lovelaceValue #-}
-- | A 'Value' containing the given quantity of Lovelace.
lovelaceValue :: Integer -> Value
lovelaceValue = singleton adaSymbol adaToken


{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces v = assetClassValueOf v (AssetClass (adaSymbol, adaToken))
--getLovelace . fromValue


{-# INLINABLE getVal #-}
getVal :: TxOut -> AssetClass -> Integer
getVal ip ac = assetClassValueOf (txOutValue ip) ac




------------------------------------------------------------------------------------------------------------------------------
-- on-chain
------------------------------------------------------------------------------------------------------------------------------


{-# INLINABLE info #-}
-- ?? needed?
info :: ScriptContext -> TxInfo
info ctx = scriptContextTxInfo ctx

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput ctx = case findOwnInput ctx of
        Nothing -> traceError "state input missing"
        Just i  -> txInInfoResolved i

{-# INLINABLE ownOutput #-}
ownOutput :: ScriptContext -> TxOut
ownOutput ctx = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one SM output"

{-# INLINABLE smDatum #-}
smDatum :: Maybe Datum -> Maybe State
smDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

{-# INLINABLE outputDatum #-}
outputDatum :: ScriptContext -> State
outputDatum ctx = case txOutDatum (ownOutput ctx) of
        NoOutputDatum-> traceError "nt"
        OutputDatumHash dh -> case smDatum $ findDatum dh (scriptContextTxInfo ctx) of
            Nothing -> traceError "hs"
            Just d  -> d
        OutputDatum d -> PlutusTx.unsafeFromBuiltinData (getDatum d)

{-# INLINABLE newLabel #-}
newLabel :: ScriptContext -> Label
newLabel ctx = label (outputDatum ctx)

{-# INLINABLE oldValue #-}
oldValue :: ScriptContext -> Value
oldValue ctx = txOutValue (ownInput ctx)

{-# INLINABLE newValue #-}
newValue :: ScriptContext -> Value
newValue ctx = txOutValue (ownOutput ctx)

{-# INLINABLE expired #-}
expired :: Deadline -> TxInfo -> Bool
expired d info = Interval.before ((POSIXTime {getPOSIXTime = d})) (txInfoValidRange info)

{-# INLINABLE checkSigned #-}
checkSigned :: PaymentPubKeyHash -> ScriptContext -> Bool
checkSigned pkh ctx = txSignedBy (scriptContextTxInfo ctx) (unPaymentPubKeyHash pkh)

{-# INLINABLE checkPayment #-}
checkPayment :: PaymentPubKeyHash -> Value -> TxInfo -> Bool
checkPayment pkh v info = case filter (\i -> (txOutAddress i == (pubKeyHashAddress (unPaymentPubKeyHash pkh)))) (txInfoOutputs info) of
    os -> any (\o -> txOutValue o == v) os

-- <> (lovelaceValue minVal)

{-# INLINABLE agdaValidator #-}
agdaValidator :: Params -> Label -> Input -> ScriptContext -> Bool
agdaValidator param oldLabel red ctx
  = case oldLabel of
        Collecting v pkh d sigs -> case red of
                                       Propose _ _ _ -> False
                                       Add sig -> checkSigned sig ctx &&
                                                    query sig (authSigs param) &&
                                                      case newLabel ctx of
                                                          Holding -> False
                                                          Collecting v' pkh' d' sigs' -> v == v' &&
                                                                                           pkh ==
                                                                                             pkh'
                                                                                             &&
                                                                                             d == d'
                                                                                               &&
                                                                                               sigs'
                                                                                                 ==
                                                                                                 insert
                                                                                                   sig
                                                                                                   sigs
                                       Pay -> count sigs >= nr param &&
                                                case newLabel ctx of
                                                    Holding -> checkPayment pkh v
                                                                 (scriptContextTxInfo ctx)
                                                                 && oldValue ctx == newValue ctx <> v
                                                    Collecting _ _ _ _ -> False
                                       Cancel -> case newLabel ctx of
                                                     Holding -> expired d (scriptContextTxInfo ctx)
                                                     Collecting _ _ _ _ -> False
        Holding -> case red of
                       Propose v pkh d -> geq (oldValue ctx) v &&
                                            case newLabel ctx of
                                                Holding -> False
                                                Collecting v' pkh' d' sigs' -> v == v' &&
                                                                                 pkh == pkh' &&
                                                                                   d == d' &&
                                                                                     sigs' == []
                       Add _ -> False
                       Pay -> False
                       Cancel -> False


--SM Validator
{-# INLINABLE mkValidator #-}
mkValidator :: Params -> State -> Input -> ScriptContext -> Bool
mkValidator param st red ctx =

    traceIfFalse "token missing from input" (getVal (ownInput ctx) (tToken st)  == 1)                 &&
    traceIfFalse "token missing from output" (getVal (ownOutput ctx) (tToken st) == 1)                &&
    traceIfFalse "failed Validation" (agdaValidator param (label st) red ctx)

{-(case (agdaValidator param (label st) red ctx) of 
      True -> True
      False -> (traceError "failed Validation"))-}
--traceIfFalse "failed validation" False --

data MultiSig
instance Scripts.ValidatorTypes MultiSig where
  type RedeemerType MultiSig = Input
  type DatumType MultiSig = State


smTypedValidator :: Params -> V2.TypedValidator MultiSig
smTypedValidator = go
  where
    go =
      V2.mkTypedValidatorParam @MultiSig
        $$(PlutusTx.compile [||mkValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = Scripts.mkUntypedValidator -- @ScriptContext @State @Input

mkAddress :: Params -> Ledger.CardanoAddress
mkAddress = validatorCardanoAddress testnet . smTypedValidator

mkOtherAddress :: Params -> Address
mkOtherAddress = V2.validatorAddress . smTypedValidator

covIdx :: CoverageIndex
covIdx = getCovIdx $$(PlutusTx.compile [||mkValidator||])

-- Thread Token
{-# INLINABLE mkPolicy #-}
mkPolicy :: Address -> TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy addr oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO                  &&
                          traceIfFalse "wrong amount minted" checkMintedAmount        &&
                          traceIfFalse "not initial state" checkDatum  
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    cs :: CurrencySymbol
    cs = ownCurrencySymbol ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False
      
    scriptOutput :: TxOut
    scriptOutput = case filter (\i -> (txOutAddress i == (addr))) (txInfoOutputs info) of
    	[o] -> o
    	_ -> traceError "not unique SM output"
    
    checkDatum :: Bool
    checkDatum = case txOutDatum scriptOutput of 
        NoOutputDatum-> traceError "nd"
        OutputDatumHash dh -> case smDatum $ findDatum dh info of
            Nothing -> traceError "nh"
            Just d  -> tToken d == AssetClass (cs, tn) && label d == Holding
        OutputDatum dat -> case PlutusTx.unsafeFromBuiltinData @State (getDatum dat) of
            d -> tToken d == AssetClass (cs, tn) && label d == Holding 
            _ -> traceError "?"


policy :: Params -> TxOutRef -> TokenName -> V2.MintingPolicy
policy p oref tn = Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \addr' oref' tn' -> Scripts.mkUntypedMintingPolicy $ mkPolicy addr' oref' tn' ||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 (mkOtherAddress p)
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 oref
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 tn


curSymbol :: Params -> TxOutRef -> TokenName -> CurrencySymbol
curSymbol p oref tn = Ledger.scriptCurrencySymbol $ (Ledger.Versioned (policy p oref tn) Ledger.PlutusV2)

mintingHash :: Params -> TxOutRef -> TokenName -> Ledger.MintingPolicyHash
mintingHash p oref tn = Ledger.mintingPolicyHash $ (Ledger.Versioned (policy p oref tn) Ledger.PlutusV2)
-}

{-

-}