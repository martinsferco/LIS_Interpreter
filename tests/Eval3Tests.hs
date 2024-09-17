module Eval3Tests where

import Test.HUnit

import Data.Map (fromList, empty)
import Eval3 (eval)
import Parser (parseComm)
import AST (Comm, Error(..))

-- | Tests cases definition

testSkip :: Comm -> Test
testSkip comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on skip"
    res = Right (empty, "")


testSqrt :: Comm -> Test
testSqrt comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on sqrt computation"
    res = Right ((fromList [("i",5),("n",25),("t",25)]), 
                  "(Let n 25)(Let i -1)(Let i 0)(Let t 0)(Let i 1)(Let t 1)(Let i 2)(Let t 4)(Let i 3)(Let t 9)(Let i 4)(Let t 16)(Let i 5)(Let t 25)")


testFib :: Comm -> Test
testFib comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on fib computation"
    res = Right (fromList [("n",4), ("a",2), ("b", 3), ("i",5), ("temp",3), ("result",3)],
                 "(Let n 4)(Let a 0)(Let b 1)(Let i 2)(Let temp 1)(Let a 1)(Let b 1)(Let i 3)(Let temp 2)(Let a 1)(Let b 2)(Let i 4)(Let temp 3)(Let a 2)(Let b 3)(Let i 5)(Let result 3)")


testError1 :: Comm -> Test
testError1 comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on error1"
    res = Left DivByZero


testError2 :: Comm -> Test
testError2 comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on error2"
    res = Left UndefVar

-- | Tests cases

tests :: [(Comm -> Test, FilePath)]
tests =
    [
      (testSkip,   "ejemplos/skip.lis")
    , (testSqrt,   "ejemplos/sqrt.lis")
    , (testFib,    "ejemplos/fib.lis")
    , (testError1, "ejemplos/error1.lis")
    , (testError2, "ejemplos/error2.lis")
    ]
-- | Run tests

eval3Tests :: IO Counts
eval3Tests = do
    ts <- mapM buildTest tests
    runTestTT $ TestList ts

buildTest :: (Comm -> Test, FilePath) -> IO Test
buildTest (testSpec, fp) = do
    content <- readFile fp
    case parseComm fp content of
        Left err   -> error ("Parse error: " ++ show err)
        Right comm -> return (testSpec comm)
