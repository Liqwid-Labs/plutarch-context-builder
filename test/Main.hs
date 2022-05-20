{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Plutarch.Context.Config
import Plutarch.Context.Spending
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value

import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck 

import Control.Applicative (liftA2)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Hash (sha2)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

-- SpendingBuilderElements is a one to one representation of Spending
-- Builder API. 
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

-- Translate API representation into actual API.
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

-- Arbitrary instance for SpendingBuilderElements with Unit as datum.
-- It is required to use the free-shrinker of `Arbirary a => Arbitrary (List a)`
instance Arbitrary (SpendingBuilderElements ()) where
    arbitrary = do
      -- These shouldn't effect performance since these are Lazy. I think...
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

-- Generates tuple of required information needed for ScriptContext generation.
genBuilderElements :: Gen ([SpendingBuilderElements ()], ValidatorUTXO ())
genBuilderElements = (,) <$> listOf1 arbitrary <*> (genAnyValue >>= pure . ValidatorUTXO ())

-- shrinker for BuilderElements generator, it only uses the free-shrinker of list.
shrinkBuilderElements ::
    ([SpendingBuilderElements ()], ValidatorUTXO ()) ->
    [([SpendingBuilderElements ()], ValidatorUTXO ())]
shrinkBuilderElements (elms, v) = (,v) <$> shrink elms

-- build ScriptContext from given list of SpendingBuilderElements and
-- ValidatorUTXO--generator outputs--with default configuration.
buildContext :: ([SpendingBuilderElements ()], ValidatorUTXO ()) -> Maybe ScriptContext
buildContext (xs, vutxo) = spendingContext defaultConfig builder vutxo
  where
    builder = mconcat $ toSpendingBuilder <$> xs

-- Check if given SpendingBuilderElements is correctly represented
-- in the ScriptContext. Return True if it's correct, False otherwise.
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
    go (InputSelfExtra val _datum) =
        elem (vhashToAddr . configValidatorHash $ defaultConfig, val) inAddrVal
    go (OutputToPubKey (pkToAddr -> addr) val) =
        elem (addr, val) outAddrVal
    go (OutputToPubKeyWith (pkToAddr -> addr) val ()) =
        elem (addr, val) outAddrVal
    go (OutputToOtherScript (vhashToAddr -> addr) val ()) =
        elem (addr, val) outAddrVal
    go (OutputToValidator val _datum) =
        elem (vhashToAddr . configValidatorHash $ defaultConfig, val) outAddrVal
    go (Mint _val) = True
    go (SignedWith pk) =
        let signers = txInfoSignatories . scriptContextTxInfo $ context
         in elem pk signers
    go (ExtraData ()) = True

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
    go = max 10_000
