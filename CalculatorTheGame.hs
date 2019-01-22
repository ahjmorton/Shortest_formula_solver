module CalculatorTheGame (solutions) where

import Model
import Control.Monad (replicateM)

combinations :: Int -> [a] -> [[a]]
combinations moves values = replicateM moves values

solutions :: Num a => Int -> [Operation a] -> a -> [Formula a]
solutions 0 _ _ = []
solutions moves availableOps start =  
  [(Result start formula) | formula <- combinations moves availableOps] ++ 
   solutions (moves - 1) availableOps start

