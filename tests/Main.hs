module Main where

import Test.HUnit
import ParserTests (parserTests)
import Eval1Tests (eval1Tests)


main :: IO Counts
main = do
    putStrLn "-------------------- Parser tests --------------------"
    parserTests
    putStrLn "-------------------- Eval 1 tests --------------------"
    eval1Tests
    