{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Applicative (liftA2)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Hash (sha2)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Api.V1 (datumHash)
import Plutarch.Context.Config
import Plutarch.Context.Spending
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck

-- SpendingBuilderElement is a one to one representation of Spending
-- Builder API.
data SpendingBuilderElement
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
    deriving stock (Show, Eq)

-- Translate API representation into actual API.
toSpendingBuilder :: forall redeemer. SpendingBuilderElement -> SpendingBuilder Integer redeemer
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

-- Arbitrary instance for SpendingBuilderElement with Unit as datum.
-- It is required to use the free-shrinker of `Arbirary a => Arbitrary (List a)`
instance Arbitrary SpendingBuilderElement where
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
genBuilderElements :: Gen ([SpendingBuilderElement], ValidatorUTXO Integer)
genBuilderElements = (,) <$> listOf1 arbitrary <*> (genAnyValue >>= pure . ValidatorUTXO 0)

-- shrinker for BuilderElements generator, it only uses the free-shrinker of list.
shrinkBuilderElements ::
    ([SpendingBuilderElement], ValidatorUTXO Integer) ->
    [([SpendingBuilderElement], ValidatorUTXO Integer)]
shrinkBuilderElements (elms, v) = (,v) <$> shrink elms

-- build ScriptContext from given list of SpendingBuilderElement and
-- ValidatorUTXO--generator outputs--with default configuration.
buildContext :: ([SpendingBuilderElement], ValidatorUTXO Integer) -> Maybe ScriptContext
buildContext (xs, vutxo) = spendingContext defaultConfig builder vutxo
  where
    builder = mconcat $ toSpendingBuilder <$> xs

-- Check if given SpendingBuilderElement is correctly represented
-- in the ScriptContext. Return True if it's correct, False otherwise.
rules :: ScriptContext -> ([SpendingBuilderElement], ValidatorUTXO Integer) -> Property
rules context (spes, ValidatorUTXO vdat vval) = property $ validatorInput .&&. (conjoin $ go <$> spes)
  where
    validatorInput = property $
          checkWithDatum (validatorAddress, vval, vdat) ins
          && datumExists vdat
          
    ins = txInInfoResolved <$> (txInfoInputs . scriptContextTxInfo $ context)
    outs = txInfoOutputs . scriptContextTxInfo $ context
    datumPairs = txInfoData . scriptContextTxInfo $ context
    validatorAddress = vhashToAddr . configValidatorHash $ defaultConfig

    pkToAddr = (flip Address Nothing) . PubKeyCredential
    vhashToAddr = (flip Address Nothing) . ScriptCredential

    -- Arbitrary data to DatumHash helper
    toDatumHash :: (ToData a) => a -> DatumHash
    toDatumHash = datumHash . Datum . toBuiltinData

    -- Search given DatumHash in TxInfoData. Returns the matching data
    -- if exists.
    searchDatum :: DatumHash -> Maybe Data
    searchDatum dh
        | null filtered = Nothing
        | otherwise = Just . toData . snd . head $ filtered
      where
        filtered = filter ((dh ==) . fst) datumPairs

    -- search given address, value pair in given list of TxOut
    check :: (Address, Value) -> [TxOut] -> Bool
    check (addr, val) os
        | null filtered = False
        | otherwise = True
      where
        filtered =
            filter
                ( \x ->
                    addr == txOutAddress x
                        && val == txOutValue x
                )
                os

    checkWithDatum :: (ToData a) => (Address, Value, a) -> [TxOut] -> Bool
    checkWithDatum (addr, val, dat) os
        | null filtered = False
        | otherwise = True
      where
        filtered =
            filter
                ( \x ->
                    addr == txOutAddress x
                        && val == txOutValue x
                        && maybe False (toDatumHash dat ==) (txOutDatumHash x)
                )
                os

    -- Check if given data is currectly presented in ScriptContext.
    datumExists :: (FromData a, Eq a, ToData a) => a -> Bool
    datumExists val =
        case searchDatum dh of
            Just (fromData -> Just x) -> x == val
            _ -> False
      where
        dh = toDatumHash val

    go (InputFromPubKey (pkToAddr -> addr) val) =
        property $
            check (addr, val) ins
    go (InputFromPubKeyWith (pkToAddr -> addr) val dat) =
        property $
            checkWithDatum (addr, val, dat) ins
                && datumExists dat
    go (InputFromOtherScript (vhashToAddr -> addr) val dat) =
        property $
            checkWithDatum (addr, val, dat) ins
                && datumExists dat
    go (InputSelfExtra val dat) =
        property $
            checkWithDatum (validatorAddress, val, dat) ins
                && datumExists dat
    go (OutputToPubKey (pkToAddr -> addr) val) =
        property $
            check (addr, val) outs
    go (OutputToPubKeyWith (pkToAddr -> addr) val dat) =
        property $
            checkWithDatum (addr, val, dat) outs
                && datumExists dat
    go (OutputToOtherScript (vhashToAddr -> addr) val dat) =
        property $
            checkWithDatum (addr, val, dat) outs
                && datumExists dat
    go (OutputToValidator val dat) =
        property $
            checkWithDatum (validatorAddress, val, dat) outs
                && datumExists dat
    go (Mint _val) = property $ True
    go (SignedWith pk) =
        property $
            let signers = txInfoSignatories . scriptContextTxInfo $ context
             in elem pk signers
    go (ExtraData dat) =
        property $
            datumExists dat

correctInputsAndOutputs :: Property
correctInputsAndOutputs = forAllShrink genBuilderElements shrinkBuilderElements go
  where
    go elms = case buildContext elms of
        Nothing -> property False
        Just context -> rules context $ elms 

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
