module Model (Op(..), Operation, Formula(..), apply, evaluate, addOp, firstResult) where

data Op a = Op { sign::String, function::a -> a -> a }
type Operation a = (Op a, a)

instance Show (Op a) where 
   show (Op sign _) = sign

data Formula a = Result a [Operation a] 

instance Show a => Show (Formula a) where
    show (Result start ops) = unwords $ (show start):(map (\op -> show (fst op) ++ " " ++ show (snd op)) ops)

apply :: a -> Operation a -> a
apply left ((Op _ function), right) = function left right

evaluate :: Formula a -> a
evaluate (Result start ops) = foldl apply start ops

addOp :: Formula a -> Operation a -> Formula a
addOp (Result start ops) op = (Result start (ops ++ [op]))

firstResult :: a -> Formula a
firstResult start = Result start []
