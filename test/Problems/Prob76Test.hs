{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob76Test where

import Problems.Prob76

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 190569291
