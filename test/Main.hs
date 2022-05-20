module Main (main) where

import Plutarch.Context.Spending
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

import Control.Applicative
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Hash (sha2)

data SpendingBuilderElements datum
  = InputFromPubKey PubKeyHash Value
  | InputFromPubKeyWith PubKeyHash Value ()
  | InputFromOtherScript ValidatorHash Value ()
  | InputSelfExtra Value datum
  | OutputToPubKey PubKeyHash Value
  | OutputToPubKeyWith PubKeyHash Value ()
  | OutputToOtherScript ValidatorHash Value ()
  | OutputToValidator Value datum
  | Mint Value
  | SignedWith PubKeyHash
  | ExtraData ()
  deriving stock (Show)

mkBuilderElement :: forall datum redeemer. SpendingBuilderElements datum -> SpendingBuilder datum redeemer
mkBuilderElement (InputFromPubKey pk val) = inputFromPubKey pk val
mkBuilderElement (InputFromPubKeyWith pk val ()) = inputFromPubKeyWith pk val ()
mkBuilderElement (InputFromOtherScript valhash val ()) = inputFromOtherScript valhash val ()
mkBuilderElement (InputSelfExtra val datum) = inputSelfExtra val datum
mkBuilderElement (OutputToPubKey pk val) = outputToPubKey pk val
mkBuilderElement (OutputToPubKeyWith pk val ()) = outputToPubKeyWith pk val ()
mkBuilderElement (OutputToOtherScript valhash val ()) = outputToOtherScript valhash val ()
mkBuilderElement (OutputToValidator val datum) = outputToValidator val datum
mkBuilderElement (Mint val) = mint val
mkBuilderElement (SignedWith pk) = signedWith pk
mkBuilderElement (ExtraData ()) = extraData ()

genHashByteString :: Gen C.ByteString
genHashByteString = sha2 . C.pack . show <$> (chooseAny :: Gen Integer)

genValue :: AssetClass -> Gen Value
genValue ac = assetClassValue ac . abs <$> (arbitrary :: Gen Integer)

genPrettyByteString :: Gen C.ByteString
genPrettyByteString = C.pack <$> (listOf1 $ elements ['a' .. 'z'])

genAssetClass :: Gen AssetClass
genAssetClass =
  AssetClass
    <$> liftA2
      (,)
      (currencySymbol <$> genHashByteString)
      (tokenName <$> genPrettyByteString)

genAnyValue :: Gen Value
genAnyValue = genAssetClass >>= genValue

genBuilderElement :: Gen (SpendingBuilderElements ())
genBuilderElement = do
  pk <- PubKeyHash . toBuiltin <$> genHashByteString
  valhash <- ValidatorHash . toBuiltin <$> genHashByteString
  val <- genAnyValue

  elements [ InputFromPubKey pk val
           , InputFromPubKeyWith pk val ()
           , InputFromOtherScript valhash val ()
           , InputSelfExtra val ()
           , OutputToPubKey pk val
           , OutputToPubKeyWith pk val ()
           , OutputToOtherScript valhash val ()
           , OutputToValidator val ()
           , Mint val
           , SignedWith pk
           , ExtraData ()
           ]

_used :: Gen (SpendingBuilderElements ())
_used = genBuilderElement

_used2 :: forall datum redeemer. SpendingBuilder datum redeemer
_used2 = mkBuilderElement undefined

main :: IO ()
main = defaultMain
  $ testGroup "context builder"
  [
  ]
