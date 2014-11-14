{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob7Test where

import Problems.Prob7

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 104743
