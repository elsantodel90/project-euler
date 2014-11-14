{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Problems.Prob1Test
import {-@ HTF_TESTS @-} Problems.Prob2Test

main :: IO()
main = htfMain htf_importedTests
