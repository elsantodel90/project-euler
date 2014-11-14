{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob122Test where

import Problems.Prob122

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 1582
