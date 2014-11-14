{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob2Test where

import Problems.Prob2

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 4613732
