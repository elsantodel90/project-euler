{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Prob1Test where

import Prob1

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 233168

test_really_works :: Assertion
test_really_works = assertEqual answer 233168