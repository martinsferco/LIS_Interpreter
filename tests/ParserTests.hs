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

-- | Tests cases

tests :: [(FilePath -> String -> Test, FilePath)]
tests =
    [
      (testSkip, "ejemplos/skip.lis")
    , (testSqrt, "ejemplos/sqrt.lis")
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