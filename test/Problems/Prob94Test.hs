{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob94Test where

import Problems.Prob94

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 518408346
