{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob24Test where

import Problems.Prob24

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 2783915460
