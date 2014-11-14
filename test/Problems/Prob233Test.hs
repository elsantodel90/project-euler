{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob233Test where

import Problems.Prob233

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 271204031455541309
