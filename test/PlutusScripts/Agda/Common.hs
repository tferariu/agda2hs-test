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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
--{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

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
import PlutusTx.Show --qualified as S
import PlutusTx.Builtins.Class as B
--import PlutusTx.Builtins as BI
--import Data.Text qualified as Text

--import Prelude (Show (..), String)

import Prelude qualified as Haskell

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
smDatum :: Maybe Datum -> State
smDatum md = case md of
    Nothing -> traceError "no datum"
    Just (Datum d) -> P.unsafeFromBuiltinData d

{-# INLINEABLE outputDatum #-}
outputDatum :: ScriptContext -> State
outputDatum ctx = case txOutDatum (ownOutput ctx) of
  NoOutputDatum -> traceError "nt"
  OutputDatumHash dh -> smDatum $ findDatum dh (scriptContextTxInfo ctx) 
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
          (  traceIfFalse "surely not" (checkSigned sig ctx)
          && traceIfFalse "here??" (query sig (authSigs param))
          && case newLabel ctx of
            Holding -> False
            Collecting v' pkh' d' sigs' -> 
                 traceIfFalse "value?" (v == v')
              && traceIfFalse "target?" (pkh == pkh')
              && traceIfFalse "deadline?" (d == d')
              && traceIfFalse "sigs?" (sigs' == insert sig sigs)) 
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
        Holding -> traceIfFalse (show d) (expired d (scriptContextTxInfo ctx)) -- <> P.show (ifFrom (txInfoValidRange (scriptContextTxInfo ctx)))
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



{-# INLINEABLE countOutput #-}
countOutput :: ScriptContext -> TxOut
countOutput ctx = case getContinuingOutputs ctx of
  [o] -> o
  _ -> traceError "expected exactly one SM output"

{-# INLINEABLE cDatum #-}
cDatum :: Maybe Datum -> [Integer]
cDatum md = case md of
    Nothing -> traceError "no datum"
    Just (Datum d) -> P.unsafeFromBuiltinData d

{-# INLINEABLE countDatum #-}
countDatum :: ScriptContext -> [Integer]
countDatum ctx = case txOutDatum (countOutput ctx) of
  NoOutputDatum -> traceError "nt"
  OutputDatumHash dh -> cDatum $ findDatum dh (scriptContextTxInfo ctx) 
  OutputDatum d -> P.unsafeFromBuiltinData (getDatum d)


{-# INLINEABLE weirdInsert #-}
weirdInsert :: Integer -> [Integer] -> [Integer]
weirdInsert i [] = [i]
weirdInsert i (x:xs) = if i == x then (x:xs)
                        else (x:(weirdInsert i xs))

{-# INLINEABLE mlcValidator #-}
mlcValidator :: [Integer] -> Integer -> ScriptContext -> Bool
mlcValidator dat red ctx = 
    traceIfFalse "failed Validation" (checkCount)
  where
    checkCount ::Bool
    checkCount = weirdInsert red dat == countDatum ctx

{-# INLINEABLE memValidator #-}
memValidator :: () -> [PubKeyHash] -> ScriptContext -> Bool
memValidator () red ctx = 
    traceIfFalse (show red) True



-- Thread Token
{-# INLINEABLE mkPolicy #-}
mkPolicy :: Address -> TxOutRef -> () -> ScriptContext -> Bool
mkPolicy addr oref () ctx = traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "wrong amount minted" checkMintedAmount
    && traceIfFalse "not initial state" checkDatum

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    tn :: TokenName
    tn = TokenName "ThreadToken"

    cs :: CurrencySymbol
    cs = ownCurrencySymbol ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(_, tn', amt)] -> amt == 1 && tn' == tn
      _ -> False

    scriptOutput :: TxOut
    scriptOutput = case filter (\i -> (txOutAddress i == (addr))) (txInfoOutputs info) of
      [o] -> o
      _ -> traceError "not unique SM output"

    checkDatum :: Bool
    checkDatum = case txOutDatum scriptOutput of
      NoOutputDatum -> traceError "nd"
      OutputDatumHash dh -> case smDatum $ findDatum dh info of
        d -> tToken d == AssetClass (cs, tn) && label d == Holding
      OutputDatum dat -> case P.unsafeFromBuiltinData @State (getDatum dat) of
        d -> tToken d == AssetClass (cs, tn) && label d == Holding

