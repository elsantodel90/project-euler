{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob99Test where

import Problems.Prob99

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = answer_calculator >>= assertEqual 709
