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

intexp :: Parser (Exp Int)
intexp = chainl1 termParser addMinusOp

addMinusOp :: Parser (Exp Int -> Exp Int -> Exp Int)
addMinusOp = try ( do { reservedOp lis "+" ; return Plus } ) <|>
                   do { reservedOp lis "-" ; return Minus }


termParser :: Parser (Exp Int)
termParser = chainl1 factorParser mulDivOp

mulDivOp :: Parser (Exp Int -> Exp Int -> Exp Int)
mulDivOp = try ( do { reservedOp lis "*" ; return Times } ) <|>
                 do { reservedOp lis "/" ; return Div }
                 

factorParser :: Parser (Exp Int)
factorParser =  try (do reservedOp lis "-"
                        f <- factorParser
                        return (UMinus f))
                <|> atomParser


{- 
  En el parser atomParser, podriamos haber optimizado la funcion para que cuando
  encontrara una variable, ahi se fijara si podia encontrar ++. -- o nada. De esta
  manera no teniamos que volver a leer el identificador de la variable en cada
  intento. No lo hicimos ya que consideramos que de la manera que lo hicimos 
  era mucho mas legible y el codigo se entendia mucho mejor
-}
atomParser :: Parser (Exp Int)
atomParser = try (parens lis intexp) <|> 
             (try ( do { i <- identifier lis ; reservedOp lis "++" ; return (VarInc i) } )) <|> 
             (try ( do { i <- identifier lis ; reservedOp lis "--" ; return (VarDec i) } )) <|>
             (try ( do { i <- identifier lis ; return (Var i) } ))                          <|>
                  ( do { n <- natural lis    ; return (Const (fromInteger n)) })


------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 orTermParser ( do { reservedOp lis "||" ; return Or } )

orTermParser :: Parser (Exp Bool)
orTermParser = chainl1 andTermParser ( do { reservedOp lis "&&" ; return And } )

andTermParser :: Parser (Exp Bool)
andTermParser = try (do reservedOp lis "!"
                        ap <- andTermParser
                        return (Not ap))
                
                <|> atomBoolParser

atomBoolParser :: Parser (Exp Bool)
atomBoolParser = (try (parens lis boolexp))                                                              <|> 
                 (try ( do { e1 <- intexp ; reservedOp lis "==" ; e2 <- intexp ; return (Eq e1 e2)  } )) <|>
                 (try ( do { e1 <- intexp ; reservedOp lis "!=" ; e2 <- intexp ; return (NEq e1 e2) } )) <|>
                 (try ( do { e1 <- intexp ; reservedOp lis "<"  ; e2 <- intexp ; return (Lt e1 e2)  } )) <|>
                 (try ( do { e1 <- intexp ; reservedOp lis ">"  ; e2 <- intexp ; return (Gt e1 e2)  } )) <|>
                 (try ( do { reserved lis "false" ; return BFalse } ))                                   <|>
                      ( do { reserved lis "true"  ; return BTrue  } )

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 stmtParser ( do reservedOp lis ";" ; return Seq)


stmtParser :: Parser Comm
stmtParser = (try ( do { reserved lis "skip" ; return Skip } ))                                                 <|> 
             (try ( do { i <- identifier lis ; reservedOp lis "=" ; e <- intexp ; return (Let i e) } ))         <|> 
             (try ( do { reserved lis "repeat" ; c <- (braces lis comm) ; reserved lis "until" ; b <- boolexp ;
                         return (RepeatUntil c b) } ))                                                          <|>
             (try ( do { reserved lis "if" ; b <- boolexp ; c1 <- (braces lis comm) ; reserved lis "else" ;
                         c2 <- (braces lis comm) ; return (IfThenElse b c1 c2) } ))                             <|>
                  ( do { reserved lis "if" ; b <- boolexp ; c  <- (braces lis comm) ; return (IfThen b c) })

------------------------------------
-- Función de parseo
------------------------------------

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
