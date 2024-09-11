module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
initState :: State
initState = (M.empty, "") 

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor = undefined

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = undefined

-- Agrega una traza dada al estado
-- Completar la definición
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
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm = undefined

-- Evalúa una expresión
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp = undefined
