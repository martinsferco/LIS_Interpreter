module Eval1Tests where

import Test.HUnit

-- | Run tests

eval1Tests :: IO Counts
eval1Tests = do
    -- Add tests
    runTestTT $ TestList []
