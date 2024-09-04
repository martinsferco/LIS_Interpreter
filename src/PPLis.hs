{-# OPTIONS_GHC  -Wno-overlapping-patterns #-}

module PPLis where

import           AST
import           Text.PrettyPrint
import           Prelude                 hiding ( (<>) )

tabW :: Int
tabW = 2

pVar :: Variable -> Doc
pVar = text

pExp :: Exp a -> Doc
pExp (Const  i ) = int i
pExp (Var    x ) = pVar x
pExp (UMinus n ) = text "-" <+> pExp n
pExp (Plus  a b) = pExp a <+> text "+" <+> pExp b
pExp (Times a b) = pExpMaybeParen a <+> text "*" <+> pExpMaybeParen b
pExp (Minus a b) = pExp a <+> text "-" <+> pExpMaybeParen b
pExp (Div   a b) = pExpMaybeParen a <+> text "/" <+> pExpMaybeParen b
pExp BTrue       = text "true"
pExp BFalse      = text "false"
pExp (Eq  a b)   = pExp a <+> text "==" <+> pExp b
pExp (NEq a b)   = pExp a <+> text "!=" <+> pExp b
pExp (Lt  a b)   = pExp a <+> text "<" <+> pExp b
pExp (Gt  a b)   = pExp a <+> text ">" <+> pExp b
pExp (And a b)   = pExp a <+> text "&&" <+> pExp b
pExp (Or  a b)   = pExp a <+> text "||" <+> pExp b
pExp (Not b  )   = text "!" <+> pExp b
pExp _ =
  error
    "El Pretty Printer no está implementado para las extensiones del Ejercicio 2."

pExpMaybeParen :: Exp a -> Doc
pExpMaybeParen e@(Plus _ _)  = parens (pExp e)
pExpMaybeParen e@(Minus _ _) = parens (pExp e)
pExpMaybeParen e             = pExp e

pComm :: Comm -> Doc
pComm Skip        = text "skip"
pComm (Let x  e ) = pVar x <+> text "=" <+> pExp e
pComm (Seq c1 c2) = pComm c1 <> semi $$ pComm c2
pComm (IfThen b c) =
  text "if" <+> parens (pExp b) <+> lbrace $$ nest tabW (pComm c) $$ rbrace
pComm (IfThenElse b c1 c2) =
  text "if"
    <+> parens (pExp b)
    <+> lbrace
    $$  nest tabW (pComm c1)
    $$  rbrace
    <+> text "else"
    <+> lbrace
    $$  nest tabW (pComm c2)
    $$  rbrace
pComm (RepeatUntil c b) =
  text "repeat" <+> lbrace $$ nest tabW (pComm c) $$ rbrace <+> text "until" <+> parens (pExp b)
  
renderComm :: Comm -> String
renderComm = render . pComm

renderExp :: Exp a -> String
renderExp = render . pExp

