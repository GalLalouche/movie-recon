module Common.MaybeTsTest where

import Common.MaybeTs            (fromFoldable)
import Common.TestCommon         (Box(Box))
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List.NonEmpty        (NonEmpty((:|)))

import Test.Tasty
import Test.Tasty.HUnit


test_all = testGroup "MaybeTUtils" [
    testCase "from empty" $ do
      let result = runMaybeT $ fromFoldable $ Box ([] :: [Int])
      result @?= Box Nothing
    , testCase "from non empty" $ do
      let result = runMaybeT $ fromFoldable $ Box [1, 2, 3]
      result @?= Box (Just $ 1 :| [2, 3])
  ]
