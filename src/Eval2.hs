module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple


type ComputationResult a = Either Error (Pair a State)

-- Estados
type State = M.Map Variable Int

-- Estado vacío
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of
                Nothing -> Left UndefVar
                Just n  -> Right n


-- Cambia el valor de una variable en un estado
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
stepComm :: Comm -> State -> ComputationResult Comm
stepComm (Let v e)            s = case evalExp e s of 
                                    Left error       -> Left error
                                    Right (n :!: s') -> Right (Skip :!: update v n s')

stepComm (Seq Skip c1)        s = Right (c1 :!: s)
stepComm (Seq c0 c1)          s = case stepComm c0 s of
                                    Left error         -> Left error
                                    Right (c0' :!: s') -> Right (Seq c0' c1 :!: s')

stepComm (IfThenElse b c0 c1) s = case evalExp b s of
                                    Left error -> Left error
                                    Right (b' :!: s') -> if b' then Right (c0 :!: s')
                                                               else Right (c1 :!: s')
stepComm (RepeatUntil c b)    s = Right (Seq c c' :!: s)
                                  where c' = (IfThenElse b Skip (RepeatUntil c b)) 


-- Evalúa una expresión
evalExp :: Exp a -> State -> ComputationResult a
evalExp (Const n) s = Right (n :!: s)
evalExp (Var x)   s = case lookfor x s of
                        Left _  -> Left UndefVar
                        Right n -> Right (n :!: s)

evalExp (UMinus e)    s = unaryOperation (0-) e s
evalExp (Plus e0 e1)  s = binaryOperation (+) e0 e1 s
evalExp (Minus e0 e1) s = binaryOperation (-) e0 e1 s
evalExp (Times e0 e1) s = binaryOperation (*) e0 e1 s

-- Podriamos mejorar que no evalue dos veces e1
evalExp (Div e0 e1)   s = case evalExp e1 s of
                            Left error      -> Left error
                            Right (0 :!: _) -> Left DivByZero
                            _                 -> binaryOperation (div) e0 e1 s


-- chequear que existe la variable
evalExp (VarInc x) s = case lookfor x s of 
                          Left _  -> Left UndefVar
                          Right n -> Right (n + 1 :!: (update x (n + 1) s))

evalExp (VarDec x) s = case lookfor x s of 
                          Left _  -> Left UndefVar
                          Right n -> Right (n - 1 :!: (update x (n - 1) s))
  
  
evalExp BTrue  s = Right (True :!: s)         
evalExp BFalse s = Right (False :!: s)         

evalExp (Lt e0 e1) s = binaryOperation (<) e0 e1 s    
evalExp (Gt e0 e1) s = binaryOperation (>) e0 e1 s

evalExp (And p0 p1) s = binaryOperation (&&) p0 p1 s
evalExp (Or p0 p1)  s = binaryOperation (||) p0 p1 s
evalExp (Not p)     s = unaryOperation (not) p s
evalExp (Eq e0 e1)  s = binaryOperation (==) e0 e1 s
evalExp (NEq e0 e1) s = binaryOperation (/=) e0 e1 s




binaryOperation :: (a -> a -> b) -> Exp a -> Exp a -> State -> ComputationResult b
binaryOperation bop e0 e1 s = case evalExp e0 s of 
                                Left error0       -> Left error0
                                Right (v0 :!: s') -> case evalExp e1 s' of
                                                            Left error1        -> Left error1
                                                            Right (v1 :!: s'') -> Right (bop v0 v1 :!: s'') 


unaryOperation  :: (a -> b) -> Exp a -> State -> ComputationResult b
unaryOperation uop e s = case evalExp e s of
                            Left error       -> Left error 
                            Right (v :!: s') -> Right (uop v :!: s')
