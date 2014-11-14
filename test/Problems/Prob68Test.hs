{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Problems.Prob68Test where

import Problems.Prob68

import Test.Framework
import Test.Framework.TestInterface (Assertion)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_works :: Assertion
test_works = assertEqual answer 6531031914842725
