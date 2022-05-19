module Main (main) where

import Plutarch.Context.Spending
import Plutarch.Context.Config
import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Value as Value

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Hash (sha2)

users :: [PubKeyHash]
users = PubKeyHash . toBuiltin . sha2 . C.pack . show <$> ([1 ..] :: [Integer])

config :: ContextConfig
config = ContextConfig
         { configFee = Value.singleton Value.adaSymbol Value.adaToken 2000000
         , configTimeRange = always
         , configTxId = (TxId "abcd")
         , configCurrencySymbol = "ff"
         , configValidatorHash = "90ab"
         }
ac :: Value.AssetClass
ac = Value.assetClass "abcd" "token_name"

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
