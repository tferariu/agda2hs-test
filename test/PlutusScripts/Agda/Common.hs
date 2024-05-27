{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fobject-code #-}

module PlutusScripts.Agda.Common where

import PlutusLedgerApi.V1.Interval qualified as P
import PlutusLedgerApi.V1.Value -- qualified as P
-- import PlutusLedgerApi.V3 --qualified as PV3
import PlutusLedgerApi.V2 qualified as P

-- qualified as PV3

import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts (
  findDatum,
  findOwnInput,
  getContinuingOutputs,
  ownCurrencySymbol,
  txSignedBy,
 )

-- qualified as P
import PlutusTx qualified as P
import PlutusTx.Prelude

import Prelude (Show (..), String)

import Helpers.ScriptUtils

minVal :: Integer -- Lovelace
minVal = 2000000

type Deadline = Integer

data Label
  = Holding
  | Collecting Value PubKeyHash Deadline [PubKeyHash]
  deriving (Show)

{-
{-# INLINABLE listEq #-}
listEq :: [PubKeyHash] -> [PubKeyHash] -> Bool
listEq [] [] = True
listEq (x:xs) [] = False
listEq [] (y:ys) = False
listEq (x:xs)(y:ys) = x == y && listEq xs ys

instance Eq [PubKeyHash] where
    {-# INLINABLE (==) #-}
    a == b = listEq a b-}

{-# INLINEABLE lEq #-}
lEq :: Label -> Label -> Bool
lEq Holding Holding = True
lEq Holding (Collecting _ _ _ _) = False
lEq (Collecting _ _ _ _) Holding = False
lEq (Collecting v pkh d sigs) (Collecting v' pkh' d' sigs') = v == v' && pkh == pkh' && d == d' && sigs == sigs'

instance Eq Label where
  {-# INLINEABLE (==) #-}
  b == c = lEq b c

data State = State
  { label :: Label
  , tToken :: AssetClass
  }
  deriving (Show)

instance Eq State where
  {-# INLINEABLE (==) #-}
  b == c =
    (label b == label c)
      && (tToken b == tToken c)

data Input
  = Propose Value PubKeyHash Deadline
  | Add PubKeyHash
  | Pay
  | Cancel
  deriving (Show)

P.unstableMakeIsData ''Label
P.makeLift ''Label
P.unstableMakeIsData ''Input
P.makeLift ''Input
P.unstableMakeIsData ''State
P.makeLift ''State

{-# INLINEABLE query #-}
query :: PubKeyHash -> [PubKeyHash] -> Bool
query pkh [] = False
query pkh (x : l') = x == pkh || (query pkh l')

{-# INLINEABLE insert #-}
insert :: PubKeyHash -> [PubKeyHash] -> [PubKeyHash]
insert pkh [] = [pkh]
insert pkh (x : l') =
  if x == pkh then x : l' else x : insert pkh l'

{-# INLINEABLE count #-}
count :: [PubKeyHash] -> Integer
count [] = 0
count (x : l) = 1 + count l

data Params = Params {authSigs :: [PubKeyHash], nr :: Integer}
  deriving (Show)

P.unstableMakeIsData ''Params
P.makeLift ''Params

{-# INLINEABLE lovelaceValue #-}

-- | A 'Value' containing the given quantity of Lovelace.
lovelaceValue :: Integer -> Value
lovelaceValue = singleton adaSymbol adaToken

{-# INLINEABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces v = assetClassValueOf v (AssetClass (adaSymbol, adaToken))

-- getLovelace . fromValue

{-# INLINEABLE getVal #-}
getVal :: TxOut -> AssetClass -> Integer
getVal ip ac = assetClassValueOf (txOutValue ip) ac

------------------------------------------------------------------------------------------------------------------------------
-- on-chain
------------------------------------------------------------------------------------------------------------------------------

{-# INLINEABLE info #-}
-- ?? needed?
info :: ScriptContext -> TxInfo
info ctx = scriptContextTxInfo ctx

{-# INLINEABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput ctx = case findOwnInput ctx of
  Nothing -> traceError "state input missing"
  Just i -> txInInfoResolved i

{-# INLINEABLE ownOutput #-}
ownOutput :: ScriptContext -> TxOut
ownOutput ctx = case getContinuingOutputs ctx of
  [o] -> o
  _ -> traceError "expected exactly one SM output"

{-# INLINEABLE smDatum #-}
smDatum :: Maybe Datum -> Maybe State
smDatum md = do
  Datum d <- md
  P.fromBuiltinData d

{-# INLINEABLE outputDatum #-}
outputDatum :: ScriptContext -> State
outputDatum ctx = case txOutDatum (ownOutput ctx) of
  NoOutputDatum -> traceError "nt"
  OutputDatumHash dh -> case smDatum $ findDatum dh (scriptContextTxInfo ctx) of
    Nothing -> traceError "hs"
    Just d -> d
  OutputDatum d -> P.unsafeFromBuiltinData (getDatum d)

{-# INLINEABLE newLabel #-}
newLabel :: ScriptContext -> Label
newLabel ctx = label (outputDatum ctx)

{-# INLINEABLE oldValue #-}
oldValue :: ScriptContext -> Value
oldValue ctx = txOutValue (ownInput ctx)

{-# INLINEABLE newValue #-}
newValue :: ScriptContext -> Value
newValue ctx = txOutValue (ownOutput ctx)

{-# INLINEABLE expired #-}
expired :: Deadline -> TxInfo -> Bool
expired d info = P.before ((POSIXTime{getPOSIXTime = d})) (txInfoValidRange info)

{-# INLINEABLE checkSigned #-}
checkSigned :: PubKeyHash -> ScriptContext -> Bool
checkSigned pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINEABLE checkPayment #-}
checkPayment :: PubKeyHash -> Value -> TxInfo -> Bool
checkPayment pkh v info = case filter (\i -> (txOutAddress i == (pubKeyHashAddress pkh))) (txInfoOutputs info) of
  os -> any (\o -> txOutValue o == v) os

-- <> (lovelaceValue minVal)

{-# INLINEABLE agdaValidator #-}
agdaValidator :: Params -> Label -> Input -> ScriptContext -> Bool
agdaValidator param oldLabel red ctx =
  case oldLabel of
    Collecting v pkh d sigs -> case red of
      Propose _ _ _ -> False
      Add sig ->
        checkSigned sig ctx
          && query sig (authSigs param)
          && case newLabel ctx of
            Holding -> False
            Collecting v' pkh' d' sigs' ->
              v
                == v'
                && pkh
                == pkh'
                && d
                == d'
                && sigs'
                == insert
                  sig
                  sigs
      Pay ->
        count sigs
          >= nr param
          && case newLabel ctx of
            Holding ->
              checkPayment
                pkh
                v
                (scriptContextTxInfo ctx)
                && oldValue ctx
                == newValue ctx
                <> v
            Collecting _ _ _ _ -> False
      Cancel -> case newLabel ctx of
        Holding -> expired d (scriptContextTxInfo ctx)
        Collecting _ _ _ _ -> False
    Holding -> case red of
      Propose v pkh d ->
        geq (oldValue ctx) v
          && case newLabel ctx of
            Holding -> False
            Collecting v' pkh' d' sigs' ->
              v
                == v'
                && pkh
                == pkh'
                && d
                == d'
                && sigs'
                == []
      Add _ -> False
      Pay -> False
      Cancel -> False

-- SM Validator
{-# INLINEABLE mkValidator #-}
mkValidator :: Params -> State -> Input -> ScriptContext -> Bool
mkValidator param st red ctx =
  traceIfFalse "token missing from input" (getVal (ownInput ctx) (tToken st) == 1)
    && traceIfFalse "token missing from output" (getVal (ownOutput ctx) (tToken st) == 1)
    && traceIfFalse "failed Validation" (agdaValidator param (label st) red ctx)

{-
{-# INLINEABLE mkSmValidator #-}
mkSmValidator :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
mkSmValidator =

class (PV1.UnsafeFromData sc) => IsScriptContext sc where
  {-# INLINEABLE mkUntypedValidator #-}
  mkUntypedValidator
    :: (PV1.UnsafeFromData d, PV1.UnsafeFromData r)
    => (d -> r -> sc -> Bool)
    -> UntypedValidator
  -- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
  mkUntypedValidator f d r p =
    P.check $
      f
        (tracedUnsafeFrom "Data decoded successfully" d)
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)-}

-- Thread Token
{-# INLINEABLE mkPolicy #-}
mkPolicy :: Address -> TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy addr oref tn () ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "wrong amount minted" checkMintedAmount
    && traceIfFalse "not initial state" checkDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    cs :: CurrencySymbol
    cs = ownCurrencySymbol ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(_, tn', amt)] -> amt == 1 -- && tn' == tn
      _ -> False

    scriptOutput :: TxOut
    scriptOutput = case filter (\i -> (txOutAddress i == (addr))) (txInfoOutputs info) of
      [o] -> o
      _ -> traceError "not unique SM output"

    checkDatum :: Bool
    checkDatum = case txOutDatum scriptOutput of
      NoOutputDatum -> traceError "nd"
      OutputDatumHash dh -> case smDatum $ findDatum dh info of
        Nothing -> traceError "nh"
        Just d -> tToken d == AssetClass (cs, tn) && label d == Holding
      OutputDatum dat -> case P.unsafeFromBuiltinData @State (getDatum dat) of
        d -> tToken d == AssetClass (cs, tn) && label d == Holding
        _ -> traceError "?"

{-
-- Mint token name policy --

{-# INLINEABLE mkMintTokenNamePolicyV3 #-}
mkMintTokenNamePolicyV3 :: P.TokenName -> PV3.ScriptContext -> Bool
mkMintTokenNamePolicyV3 tn ctx = P.traceIfFalse "wrong token name" checkTokenName
  where
    info :: PV3.TxInfo
    info = PV3.scriptContextTxInfo ctx

    -- TODO: Use builtin when available in PV3
    ownCurrencySymbol :: PV3.ScriptContext -> PV3.CurrencySymbol
    ownCurrencySymbol PV3.ScriptContext{PV3.scriptContextPurpose = PV3.Minting cs} = cs
    ownCurrencySymbol _ = P.traceError "Lh"

    checkTokenName :: Bool
    checkTokenName = P.valueOf (PV3.txInfoMint info) (ownCurrencySymbol ctx) tn P.> 0

-- Time range policy --

{-# INLINEABLE mkTimeRangePolicyV3 #-}
mkTimeRangePolicyV3 :: P.POSIXTime -> PV3.ScriptContext -> Bool
mkTimeRangePolicyV3 dl ctx = (P.to dl) `P.contains` range
  where
    info :: PV3.TxInfo
    info = PV3.scriptContextTxInfo ctx

    range :: P.POSIXTimeRange
    range = PV3.txInfoValidRange info

-- Witness redeemer policy --

{-# INLINEABLE mkWitnessRedeemerPolicyV3 #-}
mkWitnessRedeemerPolicyV3 :: P.PubKeyHash -> PV3.ScriptContext -> Bool
mkWitnessRedeemerPolicyV3 pkh ctx = P.traceIfFalse "not signed by redeemer pubkeyhash" checkWitness
  where
    info :: PV3.TxInfo
    info = PV3.scriptContextTxInfo ctx

    -- TODO: Use builtin when available in PV3
    txSignedBy :: PV3.TxInfo -> PV3.PubKeyHash -> Bool
    txSignedBy PV3.TxInfo{PV3.txInfoSignatories} k = case P.find ((P.==) k) txInfoSignatories of
      P.Just _ -> P.True
      P.Nothing -> P.False

    checkWitness :: Bool
    checkWitness = txSignedBy info pkh

-- AlwaysSucceeds minting policy --

{-# INLINEABLE mkAlwaysSucceedPolicy #-}
mkAlwaysSucceedPolicy :: P.BuiltinData -> P.BuiltinData -> ()
mkAlwaysSucceedPolicy _datum _sc = ()

-- AlwaysSucceeds validator --

{-# INLINEABLE mkAlwaysSucceedSpend #-}
mkAlwaysSucceedSpend :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
mkAlwaysSucceedSpend _datum _redeemer _sc = ()

-- AlwaysFails minting policy --

{-# INLINEABLE mkAlwaysFailsPolicy #-}
mkAlwaysFailsPolicy :: P.BuiltinData -> P.BuiltinData -> ()
mkAlwaysFailsPolicy _datum _sc = P.check $ P.error ()

-}
