{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob83Test where

import Problems.Prob83

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = answer_calculator >>= assertEqual 425185
