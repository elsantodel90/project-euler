{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob96Test where

import Problems.Prob96

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = answer_calculator >>= assertEqual 24702
