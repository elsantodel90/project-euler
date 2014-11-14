{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob4Test where

import Problems.Prob4

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 906609
