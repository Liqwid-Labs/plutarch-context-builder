module Main (main) where

import Plutarch.Context.Spending
import Plutarch.Context.Config
import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Value as Value

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Hash (sha2)

-- Instead of having random PubKeyHashes, following list of "users"
-- will be used in this example for simplicity purpose.
users :: [PubKeyHash]
users = PubKeyHash . toBuiltin . sha2 . C.pack . show <$> ([1 ..] :: [Integer])

-- We need a configuration for the context builder. Configuration
-- defines some of basic informations like transaction fee, time
-- range, and more. 
config :: ContextConfig
config = ContextConfig
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
_config = defaultConfig {configValidatorHash = "90ab"}
         
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
sb = foldr1 (<>)
     [ inputFromPubKey (users !! 1) $ Value.assetClassValue ac 2000000
     , inputFromPubKeyWith (users !! 2) (Value.assetClassValue ac 2000000) ()
     , inputSelfExtra (Value.assetClassValue ac 2000000) ()
     , outputToValidator (Value.assetClassValue ac 12345) ()
     , outputToPubKey (users !! 1) $ Value.assetClassValue ac 4000000
     ]

context :: Maybe ScriptContext
context = spendingContext
          config
          sb
          (TestUTXO () $ Value.assetClassValue ac 987986)
          

main :: IO()
main = putStrLn $ show context
