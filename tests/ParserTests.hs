module ParserTests where

import Test.HUnit

import AST
import Parser
import Text.Parsec (ParseError)

-- | Tests cases templates

okTest :: FilePath -> String -> String -> Either ParseError Comm -> Test
okTest filePath cont msg expected =
  TestCase $ assertEqual msg expected (parseComm filePath cont)


-- | Tests cases definition

testSkip :: FilePath -> String -> Test
testSkip filePath cont = okTest filePath cont "Error on skip" (Right Skip)

testSqrt :: FilePath -> String -> Test
testSqrt filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on parser for sqrt program"
    prog = Seq (Seq (Let "n" (Const 25)) 
                    (Let "i" (UMinus (Const 1)))) 
               (RepeatUntil (Seq (Let "i" (Plus (Var "i") (Const 1))) (Let "t" (Times (Var "i") (Var "i")))) 
                            (Or (Gt (Var "t") (Var "n")) (Eq (Var "t") (Var "n"))))

testError1 :: FilePath -> String -> Test
testError1 filePath cont = okTest filePath cont "Error on error1" (Right prog)
  where
    prog = Seq (Let "a" 
                      ((Times (Div (Times (Plus (Minus (Times (Const 5) (Const 5)) (Const 25)) (UMinus (Const 1))) (Const 2)) (UMinus (Const 2))) (Const 3)))) 
               (Let "a" (Div (Var "a") (Const 0)))


testError2 :: FilePath -> String -> Test
testError2 filePath cont = okTest filePath cont "Error on error2" (Right prog)
  where 
    prog = Seq (Let "a" (Const 23)) (Let "a" (Div (Var "a") (Var "c")))

testFib :: FilePath -> String -> Test
testFib filePath cont = okTest filePath cont "Error on fib" (Right prog)
  where


    prog = Seq (Let "n" (Const 4))               
               (IfThenElse (Eq (Var "n") (Const 0)) (Let "result" (Const 0)) 
               (IfThenElse (Eq (Var "n") (Const 1)) (Let "result" (Const 1)) 
               (Seq (Seq initialValues loopExpression) (Let "result" (Var "b")))))

    initialValues = (Seq (Seq (Let "a" (Const 0)) 
                              (Let "b" (Const 1))) 
                              (Let "i" (Const 2)))

    loopExpression = RepeatUntil loopBody (Gt (Var "i") (Var "n")) 
    loopBody = Seq (Seq (Seq (Let "temp" (Plus (Var "a") (Var "b")))
                             (Let "a" (Var "b")) ) 
                             (Let "b" (Var "temp"))) 
                             (Let "i" (Plus (Var "i") (Const 1))) 
      
      
-- | Tests cases

tests :: [(FilePath -> String -> Test, FilePath)]
tests =
    [
      (testSkip,   "ejemplos/skip.lis")
    , (testSqrt,   "ejemplos/sqrt.lis")
    , (testError1, "ejemplos/error1.lis")
    , (testError2, "ejemplos/error2.lis")
    , (testFib,    "ejemplos/fib.lis")
    ]

-- | Run tests

parserTests :: IO Counts
parserTests = do
    ts <- mapM buildTest tests
    runTestTT $ TestList ts

buildTest :: (FilePath -> String -> Test, FilePath) -> IO Test
buildTest (testSpec, fp) = do
    content <- readFile fp
    return (testSpec fp content)