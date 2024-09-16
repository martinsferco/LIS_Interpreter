module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

type ComputationResult a = Either Error (Pair a State)

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
initState :: State
initState = (M.empty, "") 

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor var (m, t) =  case M.lookup var m of
                        Nothing -> Left  UndefVar
                        Just n  -> Right n

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update var n (m, t) = addTrace (showLet var n) (M.insert var n m, t)

showLet :: Variable -> Int -> String
showLet var n = "Let " ++ var ++ " " ++ show n ++ "\n"


-- Agrega una traza dada al estado
addTrace :: String -> State -> State
addTrace op (m, traza) = (m, traza ++ op)  

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'


-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> ComputationResult Comm
stepComm (Let v e)            s = case evalExp e s of
                                    Left  err           -> Left   err
                                    Right (n :!: s')    -> Right (Skip :!: update v n s')     
stepComm (Seq Skip c1)        s = Right (c1 :!: s)
stepComm (Seq c0 c1)          s = case stepComm c0 s of
                                    Left  err           -> Left   err
                                    Right (c0' :!: s')  -> Right  ((Seq c0' c1) :!: s')
stepComm (IfThenElse b c0 c1) s = case evalExp b s of
                                    Left  err           -> Left   err
                                    Right (b' :!: s')   -> Right  ((if b' then c0 else c1) :!: s')  -- los parentesis del if son necesarios??
stepComm (RepeatUntil c b)    s = Right (Seq c c' :!: s)
                                  where c' = (IfThenElse b Skip (RepeatUntil c b)) 


-- Evalúa una expresión
evalExp :: Exp a -> State -> ComputationResult a
evalExp (Const n)     s = Right (n :!: s)
evalExp (Var x)       s = case lookfor x s of
                            Left  err   ->  Left  err
                            Right n     ->  Right (n :!: s)

evalExp (UMinus e)    s = evalUnary (negate) e s 
evalExp (Not p)       s = evalUnary (not)    p s

evalExp (VarInc x)    s = evalIncDec (succ)  x s
evalExp (VarDec x)    s = evalIncDec (pred)  x s

evalExp BTrue         s = Right (True  :!: s)      
evalExp BFalse        s = Right (False :!: s)

evalExp (Plus e0 e1)  s = evalBin (+)   e0 e1 s
evalExp (Minus e0 e1) s = evalBin (-)   e0 e1 s
evalExp (Times e0 e1) s = evalBin (*)   e0 e1 s
evalExp (Div e0 e1)   s = evalDiv       e0 e1 s

evalExp (Lt e0 e1)    s = evalBin (<)   e0 e1 s
evalExp (Gt e0 e1)    s = evalBin (>)   e0 e1 s 

evalExp (And p0 p1)   s = evalBin (&&)  p0 p1 s
evalExp (Or p0 p1)    s = evalBin (||)  p0 p1 s
evalExp (Eq e0 e1)    s = evalBin (==)  e0 e1 s
evalExp (NEq e0 e1)   s = evalBin (/=)  e0 e1 s

evalDiv :: Exp Int -> Exp Int -> State -> ComputationResult Int
evalDiv e0 e1   s   = case evalExp e0 s of
                        Left err            ->  Left err
                        Right (e0' :!: s')  ->  case evalExp e1 s' of
                                                  Left  err           ->  Left err
                                                  Right (0   :!: _  ) ->  Left DivByZero
                                                  Right (e1' :!: s'') ->  Right (div e0' e1' :!: s'')

evalIncDec :: (Int -> Int) -> Variable -> State -> ComputationResult Int
evalIncDec op x s   = case lookfor x s of
                        Left  err   ->  Left err
                        Right v     ->  Right (v' :!: update x v' s)
                                        where  v' = op v

evalBin :: (a -> a -> b) -> Exp a -> Exp a -> State -> ComputationResult b
evalBin op e0 e1 s  = case evalExp e0 s of
                        Left err            ->  Left err
                        Right (e0' :!: s')  ->  case evalExp e1 s' of
                                                  Left err            -> Left   err
                                                  Right (e1' :!: s'') -> Right  (op e0' e1' :!: s'')


evalUnary :: (a -> a) -> Exp a -> State -> ComputationResult a
evalUnary op e s    = case evalExp e s of
                        Left err          -> Left   err
                        Right (e' :!: s') -> Right  (op e' :!: s')
