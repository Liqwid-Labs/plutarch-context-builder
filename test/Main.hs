{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Plutarch.Context.Config
import Plutarch.Context.Spending
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value

import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck

import Control.Applicative
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Hash (sha2)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

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

toSpendingBuilder :: forall datum redeemer. SpendingBuilderElements datum -> SpendingBuilder datum redeemer
toSpendingBuilder (InputFromPubKey pk val) = inputFromPubKey pk val
toSpendingBuilder (InputFromPubKeyWith pk val ()) = inputFromPubKeyWith pk val ()
toSpendingBuilder (InputFromOtherScript valhash val ()) = inputFromOtherScript valhash val ()
toSpendingBuilder (InputSelfExtra val datum) = inputSelfExtra val datum
toSpendingBuilder (OutputToPubKey pk val) = outputToPubKey pk val
toSpendingBuilder (OutputToPubKeyWith pk val ()) = outputToPubKeyWith pk val ()
toSpendingBuilder (OutputToOtherScript valhash val ()) = outputToOtherScript valhash val ()
toSpendingBuilder (OutputToValidator val datum) = outputToValidator val datum
toSpendingBuilder (Mint val) = mint val
toSpendingBuilder (SignedWith pk) = signedWith pk
toSpendingBuilder (ExtraData ()) = extraData ()

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

genBuilderElements :: Gen ([SpendingBuilderElements ()], ValidatorUTXO ())
genBuilderElements = (,) <$> listOf1 (genElement) <*> (genAnyValue >>= pure . ValidatorUTXO ())
  where
    genElement = do
        pk <- PubKeyHash . toBuiltin <$> genHashByteString
        valhash <- ValidatorHash . toBuiltin <$> genHashByteString
        val <- genAnyValue

        elements
            [ InputFromPubKey pk val
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

buildContext :: ([SpendingBuilderElements ()], ValidatorUTXO ()) -> Maybe ScriptContext
buildContext (xs, vutxo) = spendingContext defaultConfig builder vutxo
  where
    builder = foldr1 (<>) $ (toSpendingBuilder <$> xs)

rules :: ScriptContext -> SpendingBuilderElements () -> Bool
rules context spe = go spe
  where
    addrValPairs = fmap (\out -> (txOutAddress out, txOutValue out))
    inAddrVal = addrValPairs $ txInInfoResolved <$> (txInfoInputs . scriptContextTxInfo $ context)
    outAddrVal = addrValPairs $ txInfoOutputs . scriptContextTxInfo $ context
    pkToAddr = (flip Address Nothing) . PubKeyCredential
    vhashToAddr = (flip Address Nothing) . ScriptCredential

    go (InputFromPubKey (pkToAddr -> addr) val) =
        elem (addr, val) inAddrVal
    go (InputFromPubKeyWith (pkToAddr -> addr) val ()) =
        elem (addr, val) inAddrVal
    go (InputFromOtherScript (vhashToAddr -> addr) val ()) =
        elem (addr, val) inAddrVal
    go (InputSelfExtra val datum) =
        elem (vhashToAddr . configValidatorHash $ defaultConfig, val) inAddrVal
    go (OutputToPubKey (pkToAddr -> addr) val) =
        elem (addr, val) outAddrVal
    go (OutputToPubKeyWith (pkToAddr -> addr) val ()) =
        elem (addr, val) outAddrVal
    go (OutputToOtherScript (vhashToAddr -> addr) val ()) =
        elem (addr, val) outAddrVal
    go (OutputToValidator val datum) =
        elem (vhashToAddr . configValidatorHash $ defaultConfig, val) outAddrVal
    go (Mint val) = True
    go (SignedWith pk) =
        let signers = txInfoSignatories . scriptContextTxInfo $ context
         in elem pk signers
    go (ExtraData ()) = True

prop :: Property
prop = forAll genBuilderElements go
  where
    go elms = case buildContext elms of
        Nothing -> False
        Just context -> foldr1 (&&) $ rules context <$> fst elms

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain . adjustOption go $
        testGroup
            "context builder"
            [testProperty "builder builds all inputs correctly" prop]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
