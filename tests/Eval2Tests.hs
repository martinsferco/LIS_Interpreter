module Eval2Tests where

import Test.HUnit

import Data.Map (fromList, empty)
import Eval2 (eval)
import Parser (parseComm)
import AST (Comm, Error(..))

-- | Tests cases definition

testSkip :: Comm -> Test
testSkip comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on skip"
    res = Right empty


testSqrt :: Comm -> Test
testSqrt comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on sqrt computation"
    res = Right (fromList [("i",5),("n",25),("t",25)])


testFib :: Comm -> Test
testFib comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on fib computation"
    res = Right (fromList [("n",4), ("a",2), ("b", 3), ("i",5), ("temp",3), ("result",3)])


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

eval2Tests :: IO Counts
eval2Tests = do
    ts <- mapM buildTest tests
    runTestTT $ TestList ts

buildTest :: (Comm -> Test, FilePath) -> IO Test
buildTest (testSpec, fp) = do
    content <- readFile fp
    case parseComm fp content of
        Left err   -> error ("Parse error: " ++ show err)
        Right comm -> return (testSpec comm)
