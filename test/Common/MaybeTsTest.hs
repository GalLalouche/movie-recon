module Common.MaybeTsTest where

import Common.MaybeTs            (fromList)
import Common.TestCommon         (Box(..))
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List.NonEmpty        (NonEmpty((:|)))

import Test.Tasty
import Test.Tasty.HUnit


test_all = testGroup "MaybeTUtils" [
    testCase "from empty" $ do
      let result = runMaybeT $ fromList $ Box ([] :: [Int])
      result @?= Box Nothing
    , testCase "from non empty" $ do
      let result = runMaybeT $ fromList $ Box [1, 2, 3]
      result @?= Box (Just $ 1 :| [2, 3])
  ]
