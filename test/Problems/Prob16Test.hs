{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob16Test where

import Problems.Prob16

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 1366
