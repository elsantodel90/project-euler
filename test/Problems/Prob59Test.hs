{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob59Test where

import Problems.Prob59

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 107359
