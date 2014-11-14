{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob10Test where

import Problems.Prob10

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 142913828922
