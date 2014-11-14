{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob27Test where

import Problems.Prob27

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer (-59231)
