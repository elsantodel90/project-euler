{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob13Test where

import Problems.Prob13

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = answer_calculator >>= assertEqual 5537376230
