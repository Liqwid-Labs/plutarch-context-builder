module Main (main) where

import Plutarch.Context.Config
import Plutarch.Context.Spending
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Value as Value

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit

import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Hash (sha2)
import Data.Maybe

-- Instead of having random PubKeyHashes, following list of "users"
-- will be used in this example for simplicity purpose.
users :: [PubKeyHash]
users = PubKeyHash . toBuiltin . sha2 . C.pack . show <$> ([1 ..] :: [Integer])

-- We need a configuration for the context builder. Configuration
-- defines some of basic informations like transaction fee, time
-- range, and more.
config :: ContextConfig
config =
    ContextConfig
        { configFee = Value.singleton Value.adaSymbol Value.adaToken 2000000
        , configTimeRange = always
        , configTxId = (TxId "abcd")
        , configCurrencySymbol = "ff"
        , configValidatorHash = "90ab"
        }

-- `defaultConfig` is also provided so we can make some simpler
-- configuration with ease.
--
-- Here, we only set a custom validatorHash for the builder config.
_config :: ContextConfig
_config = defaultConfig{configValidatorHash = "90ab"}

ac :: Value.AssetClass
ac = Value.assetClass "abcd" "token_name"

-- Over the Base builder, there exist more specific builders for types
-- of transactions--Spending, Minting, Rewarding, and
-- Certifiying. Each of these specific builders will provides a
-- context specific, semigroup interfaces for construction. Generic
-- settings, like signers is backed up by the base builder.
--
-- Here spending context is constructed. Inputs and outputs can be
-- listed and combined with (<>) operator. When builder is complete,
-- provided construct function builds the context. As compare to
-- handling raw `ScriptContext`, these builders are much more
-- flexible.  With builders, new inputs can be added gracefully at any
-- point before the construction of context--it's very hard with raw data.
--
sb :: SpendingBuilder () ()
sb =
    foldr1
        (<>)
        [ -- [Inputs]

          -- regular input from public key. Pubkey and amount is required.
          inputFromPubKey (users !! 1) $ Value.assetClassValue ac 2000000
        , -- regular input from public key with datum.
          inputFromPubKeyWith (users !! 2) (Value.assetClassValue ac 19721121) ()
        , -- input to the address that is being validated. This input,
          -- however, is *not* targeted to be validated.
          inputSelfExtra (Value.assetClassValue ac 9575829) ()
        , -- [Outputs]

          -- regular output to Pubkey, nothing special
          outputToPubKey (users !! 1) $ Value.assetClassValue ac 48639572
        , -- regular output to Pubkey with datum
          outputToPubKeyWith (users !! 4) (Value.assetClassValue ac 92847592) ()
        , -- output to the target validator specified in the config
          outputToValidator (Value.assetClassValue ac 12345) ()
        , -- [Misc]

          -- Transaction can be signed with a given PubKey
          signedWith (users !! 11)
        , signedWith (users !! 12)
        ]

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
            (ValidatorUTXO () $ Value.assetClassValue ac 687264) -- input UTXO that is being validated.

main :: IO ()
main =
    defaultMain $
        testGroup
            "Spending"
            [ testCase "signed by" $ do
                let signers = txInfoSignatories . scriptContextTxInfo $ context
                forM_ [11, 12] $ (\a -> (elem (users !! a) signers) @? "signer not found: " <> show a)
            , testCase "PubKey input" $ do
                elem (userAddrs !! 1, Value.assetClassValue ac 2000000) inAddrVal @? "1"
                elem (userAddrs !! 2, Value.assetClassValue ac 19721121) inAddrVal @? "2"
            , testCase "PubKey output" $ do
                elem (userAddrs !! 1, Value.assetClassValue ac 48639572) outAddrVal @? "1"
                elem (userAddrs !! 4, Value.assetClassValue ac 92847592) outAddrVal @? "4"
            , testCase "Validator input" $ do
                elem (validatorAddr, Value.assetClassValue ac 687264) inAddrVal @? "validator input"
                elem (validatorAddr, Value.assetClassValue ac 9575829) inAddrVal @? "extra input"
            , testCase "Validator output" $ do
                elem (validatorAddr, Value.assetClassValue ac 12345) outAddrVal @? "validator input"
            ]
  where
    addrValPairs = fmap (\out -> (txOutAddress $ out, txOutValue out))
    inAddrVal = addrValPairs $ txInInfoResolved <$> (txInfoInputs . scriptContextTxInfo $ context)
    outAddrVal = addrValPairs $ txInfoOutputs . scriptContextTxInfo $ context
    userAddrs = (flip Address Nothing) . PubKeyCredential <$> users
    validatorAddr = scriptHashAddress "90ab"
