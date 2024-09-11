module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor x s = case M.lookup x s of
                Nothing -> Left   UndefVar
                Just n  -> Right  n

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

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
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Let v e)            s = case evalExp e s of
                                    Left  err           -> Left err
                                    Right (n :!: s')    -> Right (Skip :!: update v n s')     
stepComm (Seq Skip c1)        s = Right (c1 :!: s)
stepComm (Seq c0 c1)          s = case stepComm c0 s of
                                    Left  err           -> Left err
                                    Right (c0' :!: s')  -> Right ((Seq c0' c1) :!: s')
stepComm (IfThenElse b c0 c1) s = case evalExp b s of
                                    Left  err           -> Left   err
                                    Right (b' :!: s')   -> Right ((if b' then c0 else c1) :!: s')  -- los parentesis del if son necesarios??
stepComm (RepeatUntil c b)    s = Right (Seq c c' :!: s)
                                  where c' = (IfThenElse b Skip (RepeatUntil c b)) 


-- Evalúa una expresión
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n)     s = Right (n :!: s)
evalExp (Var x)       s = case lookfor x s of
                            Left  err   ->  Left  err
                            Right n     ->  Right (n :!: s)
evalExp (UMinus e)    s = evalUnary (negate) e s 
evalExp (Not p)       s = evalUnary (not) p s

evalExp (VarInc x)    s = evalIncDec (succ) x s
evalExp (VarDec x)    s = evalIncDec (pred) x s

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


evalDiv :: Exp Int -> Exp Int -> State -> Either Error (Pair Int State)
evalDiv e0 e1   s   = case evalExp e0 s of
                        Left err            ->  Left err
                        Right (e0' :!: s')  ->  case evalExp e1 s' of
                                                  Left  err           ->  Left err
                                                  Right (0   :!: _  ) ->  Left DivByZero
                                                  Right (e1' :!: s'') ->  Right (div e0' e1' :!: s'')

evalIncDec :: (Int -> Int) -> Variable -> State -> Either Error (Pair Int State)
evalIncDec op x s   = case lookfor x s of
                        Left  err   ->  Left err
                        Right v     ->  Right (v' :!: update x v' s)
                                        where  v' = op v

evalBin :: (a -> a -> b) -> Exp a -> Exp a -> State -> Either Error (Pair b State)
evalBin op e0 e1 s  = case evalExp e0 s of
                        Left err            ->  Left err
                        Right (e0' :!: s')  ->  case evalExp e1 s' of
                                                  Left err            -> Left   err
                                                  Right (e1' :!: s'') -> Right  (op e0' e1' :!: s'')

evalUnary :: (a -> a) -> Exp a -> State -> Either Error (Pair a State)
evalUnary op e s    = case evalExp e s of
                        Left err          -> Left   err
                        Right (e' :!: s') -> Right  (op e' :!: s')

