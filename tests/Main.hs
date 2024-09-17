module Main(main) where

import Test.HUnit
import ParserTests (parserTests)
import Eval1Tests (eval1Tests)
import Eval2Tests (eval2Tests)
import Eval3Tests (eval3Tests)


main :: IO Counts
main = do
    
    putStrLn ""
    putStrLn "-------------------- Parser tests --------------------"
    _ <- parserTests
    putStrLn "-------------------- Eval 1 tests --------------------"
    _ <- eval1Tests
    putStrLn "-------------------- Eval 2 tests --------------------"
    _ <- eval2Tests
    putStrLn "-------------------- Eval 3 tests --------------------"
    eval3Tests
    