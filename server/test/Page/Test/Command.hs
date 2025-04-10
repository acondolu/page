module Page.Test.Command (test) where

import Data.Aeson.Types (parseMaybe)
import qualified Page.Command as Command
import Test.Tasty
import Test.Tasty.QuickCheck as QC

test :: TestTree
test =
  testGroup
    "Page.Command"
    [ QC.testProperty "parse integers" prop_parseIntegerInverse
    ]

prop_parseIntegerInverse :: Integer -> Bool
prop_parseIntegerInverse x = (parseMaybe Command.parseInteger . Command.dumpInteger) x == Just x
