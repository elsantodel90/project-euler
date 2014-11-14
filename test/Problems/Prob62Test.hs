{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob62Test where

import Problems.Prob62

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 127035954683
