module Eval1Tests where

import Test.HUnit

import Data.Map (fromList, empty)
import Eval1 (eval)
import Parser (parseComm)
import AST (Comm)

-- | Tests cases definition

testSkip :: Comm -> Test
testSkip comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on skip"
    res = empty


testSqrt :: Comm -> Test
testSqrt comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on sqrt computation"
    res = fromList [("i",5),("n",25),("t",25)]


testFib :: Comm -> Test
testFib comm = TestCase $ assertEqual msg res (eval comm)
  where
    msg = "Error on fib computation"
    res = fromList [("n",4), ("a",2), ("b", 3), ("i",5), ("temp",3), ("result",3)]

-- | Tests cases

tests :: [(Comm -> Test, FilePath)]
tests =
    [
      (testSkip, "ejemplos/skip.lis")
    , (testSqrt, "ejemplos/sqrt.lis")
    , (testFib,  "ejemplos/fib.lis")
    ]

-- | Run tests

eval1Tests :: IO Counts
eval1Tests = do
    ts <- mapM buildTest tests
    runTestTT $ TestList ts

buildTest :: (Comm -> Test, FilePath) -> IO Test
buildTest (testSpec, fp) = do
    content <- readFile fp
    case parseComm fp content of
        Left err   -> error ("Parse error: " ++ show err)
        Right comm -> return (testSpec comm)
