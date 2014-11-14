{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob87Test where

import Problems.Prob87

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 1097343
