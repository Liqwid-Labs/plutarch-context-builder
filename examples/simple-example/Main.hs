module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as C (pack)
import Data.ByteString.Hash (sha2)
import Data.Maybe (fromJust)
import Plutarch.Context.Config (ContextConfig (..), defaultConfig)
import Plutarch.Context.Spending (
    SpendingBuilder,
    ValidatorUTXO (ValidatorUTXO),
    inputFromOtherScript,
    inputFromPubKey,
    inputFromPubKeyWith,
    inputSelfExtra,
    outputToOtherScript,
    outputToPubKey,
    outputToPubKeyWith,
    outputToValidator,
    signedWith,
    spendingContext,
 )
import Plutus.V1.Ledger.Address (
    Address (Address),
    scriptHashAddress,
 )
import Plutus.V1.Ledger.Api (
    Credential (PubKeyCredential, ScriptCredential),
    PubKeyHash (PubKeyHash),
    ScriptContext (scriptContextTxInfo),
    TokenName,
    TxId (TxId),
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoInputs, txInfoOutputs, txInfoSignatories),
    TxOut (txOutAddress, txOutValue),
    ValidatorHash (..),
    Value,
    always,
    toBuiltin,
 )
import qualified Plutus.V1.Ledger.Value as Value (
    adaSymbol,
    adaToken,
    singleton,
 )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?))

-- Instead of having multiple random PubKeyHashes, following list of
-- "users" will be used in this example for simplicity purpose.
users :: [PubKeyHash]
users = PubKeyHash . toBuiltin . sha2 . C.pack . show <$> ([1 ..] :: [Integer])

-- Samething, for scripts
scripts :: [ValidatorHash]
scripts = ValidatorHash . toBuiltin . sha2 . C.pack . show <$> ([1 ..] :: [Integer])

-- We need a configuration for the context builder. Configuration
-- defines some of basic informations like transaction fee, time
-- range, and more.
_config :: ContextConfig
_config =
    ContextConfig
        { configFee = Value.singleton Value.adaSymbol Value.adaToken 2_000_000
        , configTimeRange = always
        , configTxId = (TxId "eeee")
        , configCurrencySymbol = "abc"
        , configValidatorHash = "123cd"
        }

-- `defaultConfig` is also provided so we can make some simpler
-- configuration with ease.
--
-- Here, we only set a custom validatorHash for the builder config.
config :: ContextConfig
config = defaultConfig{configValidatorHash = "11aa"}

val :: TokenName -> Integer -> Value
val tn n = Value.singleton "abcd" tn n

-- There exist specific builders for types of transactions--Spending
-- and Minting. Each of these specific builders will provides a
-- context specific, semigroup interfaces for construction.

-- Here spending context is constructed. Inputs and outputs can be
-- listed and combined with (<>) operator. When builder is complete,
-- provided construct function builds the context. As compare to
-- handling raw `ScriptContext`, these builders are much more
-- flexible.  With builders, new inputs can be added gracefully at any
-- point before the construction of context--it's very hard with raw data.
--
sb :: SpendingBuilder () ()
sb =
    -- [Inputs]
    -- regular input from public key. Pubkey and amount is required.
    (inputFromPubKey (users !! 1) $ val "pubkey" 18237)
        <>
        -- regular input from public key with datum.
        (inputFromPubKeyWith (users !! 2) (val "pubkey" 384729) ())
        <>
        -- input to the address that is being validated. This input,
        -- however, is *not* targeted to be validated.
        (inputSelfExtra (val "self" 429957) ())
        <>
        -- input from ScriptCredential
        (inputFromOtherScript (scripts !! 1) (val "script" 19721121) ())
        <>
        -- [Outputs]

        -- regular output to Pubkey, nothing special
        (outputToPubKey (users !! 1) $ val "pubkey" 682834)
        <>
        -- regular output to Pubkey with datum
        (outputToPubKeyWith (users !! 4) (val "pubkey" 866720) ())
        <>
        -- output to the target validator specified in the config
        (outputToValidator (val "self" 3984798) ())
        <>
        -- output to ScriptConfidential other than validatorHash
        (outputToOtherScript (scripts !! 2) (val "script" 1829385) ())
        <>
        -- [Misc]

        -- Transaction can be signed with a given PubKey
        signedWith (users !! 11)
        <> signedWith (users !! 12)

-- The ScriptContext will be fabricated by
-- `spendingContext`. Normally, this would happend right before using
-- the `ScriptContext`. Since only one UTXO input for validator
-- exists, monoidal interface UTXO input is impossble and it needs to
-- be provided in `spendingContext`.
--
-- Notice the return type being `Maybe ScriptContext`; on some
-- occasions, context construction will fail due to internal
-- errors. However, these errors *does not* account ScriptContext being
-- a lawful context and *is not* a step-1 validation.
context :: ScriptContext
context =
    fromJust $
        spendingContext
            config -- configuration
            sb -- context builder
            (ValidatorUTXO () $ val "self" 395869) -- input UTXO that is being validated.

main :: IO ()
main =
    defaultMain $
        testGroup
            "Spending"
            [ testCase "signed by" $ do
                let signers = txInfoSignatories . scriptContextTxInfo $ context
                forM_ [11, 12] $ (\a -> (elem (users !! a) signers) @? "signer not found: " <> show a)
            , testCase "PubKey input" $ do
                elem (userAddrs !! 1, val "pubkey" 18237) inAddrVal @? "1"
                elem (userAddrs !! 2, val "pubkey" 384729) inAddrVal @? "2"
            , testCase "PubKey output" $ do
                elem (userAddrs !! 1, val "pubkey" 682834) outAddrVal @? "1"
                elem (userAddrs !! 4, val "pubkey" 866720) outAddrVal @? "4"
            , testCase "Other script in" $ do
                elem (scriptAddrs !! 1, val "script" 19721121) inAddrVal @? "1"
            , testCase "Other script out" $ do
                elem (scriptAddrs !! 2, val "script" 1829385) outAddrVal @? "2"
            , testCase "Validator input" $ do
                elem (validatorAddr, val "self" 395869) inAddrVal @? "validator input"
                elem (validatorAddr, val "self" 429957) inAddrVal @? "extra input"
            , testCase "Validator output" $ do
                elem (validatorAddr, val "self" 3984798) outAddrVal @? "validator input"
            ]
  where
    addrValPairs = fmap (\out -> (txOutAddress $ out, txOutValue out))
    inAddrVal = addrValPairs $ txInInfoResolved <$> (txInfoInputs . scriptContextTxInfo $ context)
    outAddrVal = addrValPairs $ txInfoOutputs . scriptContextTxInfo $ context
    userAddrs = (flip Address Nothing) . PubKeyCredential <$> users
    scriptAddrs = (flip Address Nothing) . ScriptCredential <$> scripts
    validatorAddr = scriptHashAddress . configValidatorHash $ config
