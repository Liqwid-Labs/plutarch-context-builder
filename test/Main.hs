{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Plutarch.Context.Config
import Plutarch.Context.Spending
import Plutarch.Api.V1 (datumHash)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value

import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck 

import Control.Applicative (liftA2)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Hash (sha2)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Debug.Trace

-- SpendingBuilderElements is a one to one representation of Spending
-- Builder API. 
data SpendingBuilderElements
    = InputFromPubKey PubKeyHash Value
    | InputFromPubKeyWith PubKeyHash Value Integer
    | InputFromOtherScript ValidatorHash Value Integer
    | InputSelfExtra Value Integer
    | OutputToPubKey PubKeyHash Value
    | OutputToPubKeyWith PubKeyHash Value Integer
    | OutputToOtherScript ValidatorHash Value Integer
    | OutputToValidator Value Integer
    | Mint Value
    | SignedWith PubKeyHash
    | ExtraData Integer
    deriving stock (Show)

-- Translate API representation into actual API.
toSpendingBuilder :: forall redeemer. SpendingBuilderElements -> SpendingBuilder Integer redeemer
toSpendingBuilder (InputFromPubKey pk val) = inputFromPubKey pk val
toSpendingBuilder (InputFromPubKeyWith pk val dat) = inputFromPubKeyWith pk val dat
toSpendingBuilder (InputFromOtherScript valhash val dat) = inputFromOtherScript valhash val dat
toSpendingBuilder (InputSelfExtra val dat) = inputSelfExtra val dat
toSpendingBuilder (OutputToPubKey pk val) = outputToPubKey pk val
toSpendingBuilder (OutputToPubKeyWith pk val dat) = outputToPubKeyWith pk val dat
toSpendingBuilder (OutputToOtherScript valhash val dat) = outputToOtherScript valhash val dat
toSpendingBuilder (OutputToValidator val dat) = outputToValidator val dat
toSpendingBuilder (Mint val) = mint val
toSpendingBuilder (SignedWith pk) = signedWith pk
toSpendingBuilder (ExtraData dat) = extraData dat

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

-- Arbitrary instance for SpendingBuilderElements with Unit as datum.
-- It is required to use the free-shrinker of `Arbirary a => Arbitrary (List a)`
instance Arbitrary SpendingBuilderElements where
    arbitrary = do
      -- These shouldn't effect performance since these are Lazy. I think...
      pk <- PubKeyHash . toBuiltin <$> genHashByteString
      valhash <- ValidatorHash . toBuiltin <$> genHashByteString
      val <- genAnyValue
      datum <- arbitrary

      elements
        [ InputFromPubKey pk val
        , InputFromPubKeyWith pk val datum
        , InputFromOtherScript valhash val datum
        , InputSelfExtra val datum
        , OutputToPubKey pk val
        , OutputToPubKeyWith pk val datum
        , OutputToOtherScript valhash val datum
        , OutputToValidator val datum
        , Mint val
        , SignedWith pk
        , ExtraData datum
        ]

-- Generates tuple of required information needed for ScriptContext generation.
genBuilderElements :: Gen ([SpendingBuilderElements], ValidatorUTXO Integer)
genBuilderElements = (,) <$> listOf1 arbitrary <*> (genAnyValue >>= pure . ValidatorUTXO 0)

-- shrinker for BuilderElements generator, it only uses the free-shrinker of list.
shrinkBuilderElements ::
    ([SpendingBuilderElements], ValidatorUTXO Integer) ->
    [([SpendingBuilderElements], ValidatorUTXO Integer)]
shrinkBuilderElements (elms, v) = (,v) <$> shrink elms

-- build ScriptContext from given list of SpendingBuilderElements and
-- ValidatorUTXO--generator outputs--with default configuration.
buildContext :: ([SpendingBuilderElements], ValidatorUTXO Integer) -> Maybe ScriptContext
buildContext (xs, vutxo) = spendingContext defaultConfig builder vutxo
  where
    builder = mconcat $ toSpendingBuilder <$> xs

-- Check if given SpendingBuilderElements is correctly represented
-- in the ScriptContext. Return True if it's correct, False otherwise.
rules :: ScriptContext -> SpendingBuilderElements -> Bool
rules context spe = go spe
  where
    addrValPairs = fmap (\out -> (txOutAddress out, txOutValue out))
    inAddrVal = addrValPairs $ txInInfoResolved <$> (txInfoInputs . scriptContextTxInfo $ context)
    outAddrVal = addrValPairs $ txInfoOutputs . scriptContextTxInfo $ context
    pkToAddr = (flip Address Nothing) . PubKeyCredential
    vhashToAddr = (flip Address Nothing) . ScriptCredential

    go (InputFromPubKey (pkToAddr -> addr) val) =
        elem (addr, val) inAddrVal
    go (InputFromPubKeyWith (pkToAddr -> addr) val dat) =
      let dh = datumHash . Datum . toBuiltinData $ dat in
        trace (show dh) $ elem (addr, val) inAddrVal
    go (InputFromOtherScript (vhashToAddr -> addr) val _int) =
        elem (addr, val) inAddrVal
    go (InputSelfExtra val _datum) =
        elem (vhashToAddr . configValidatorHash $ defaultConfig, val) inAddrVal
    go (OutputToPubKey (pkToAddr -> addr) val) =
        elem (addr, val) outAddrVal
    go (OutputToPubKeyWith (pkToAddr -> addr) val _int) =
        elem (addr, val) outAddrVal
    go (OutputToOtherScript (vhashToAddr -> addr) val _int) =
        elem (addr, val) outAddrVal
    go (OutputToValidator val _datum) =
        elem (vhashToAddr . configValidatorHash $ defaultConfig, val) outAddrVal
    go (Mint _val) = True
    go (SignedWith pk) =
        let signers = txInfoSignatories . scriptContextTxInfo $ context
         in elem pk signers
    go (ExtraData _int) = True

correctInputsAndOutputs :: Property
correctInputsAndOutputs = forAllShrink genBuilderElements shrinkBuilderElements go
  where
    go elms = case buildContext elms of
        Nothing -> False
        Just context -> and $ rules context <$> fst elms

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain . adjustOption go $
        testGroup
            "context builder"
            [ testProperty "builder inputs matches context" correctInputsAndOutputs
            ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10
