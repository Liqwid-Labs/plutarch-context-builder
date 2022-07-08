module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (
    QuickCheckTests,
    testProperty,
 )

import Spending (spendingBuilderProperty)

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain . adjustOption go $
        testGroup
            "context builder"
            [ testProperty "builder inputs matches context" spendingBuilderProperty
            ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
