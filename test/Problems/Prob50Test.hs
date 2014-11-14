{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob50Test where

import Problems.Prob50

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 997651
