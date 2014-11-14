{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob97Test where

import Problems.Prob97

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 8739992577
