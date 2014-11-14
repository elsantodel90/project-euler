{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob103Test where

import Problems.Prob103

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 20313839404245
