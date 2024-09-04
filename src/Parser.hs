module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        , "--"
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

{-

-}


intexp :: Parser (Exp Int)
intexp = chainl1 termParser addMinusOp

addMinusOp :: Parser (Exp Int -> Exp Int -> Exp Int)
addMinusOp = try (do {reservedOp lis "+" ; return Plus}) <|>
                  do {reservedOp lis "-" ; return Minus}



termParser :: Parser (Exp Int)
termParser = chainl1 factorParser mulDivOp

mulDivOp :: Parser (Exp Int -> Exp Int -> Exp Int)
mulDivOp = try (do {reservedOp lis "*" ; return Times}) <|>
                do {reservedOp lis "/" ; return Div}
                 

factorParser :: Parser (Exp Int)
factorParser =  try (do reservedOp lis "-"
                        f <- factorParser
                        return (UMinus f))
                <|> atomParser


atomParser :: Parser (Exp Int)
atomParser = try (do parens lis intexp)
             <|> 
             (try (do i <- identifier lis
                      reservedOp lis "++"
                      return (VarInc i)))
             <|> 
             (try (do i <- identifier lis
                      reservedOp lis "--"
                      return (VarDec i)))
             <|>
             (try (do i <- identifier lis
                      return (Var i)))
             <|> (do n <- natural lis
                     return (Const (fromInteger n)))
------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
