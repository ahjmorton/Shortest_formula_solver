import Model 
import qualified DkJrMath 
import qualified CalculatorTheGame 

import Data.List (sortBy)

isCorrect :: Eq a => a -> Formula a -> Bool
isCorrect destination formula = evaluate formula == destination

workingSolutions :: Eq a => [Formula a] -> a -> [Formula a]
workingSolutions possibleSolutions destination =
   filter (isCorrect destination) possibleSolutions

shortestFormula :: Formula a -> Formula a -> Ordering
shortestFormula left right 
   | formulaLength left < formulaLength right = GT
   | formulaLength left > formulaLength right = LT
   | otherwise = EQ

findShortestSolution :: Eq a => [Formula a] -> a -> Formula a
findShortestSolution resultSet destination = 
   head $ sortBy shortestFormula $ workingSolutions resultSet destination

findSolution :: Eq a => [Formula a] -> a -> Formula a
findSolution resultSet destination = 
    head $ workingSolutions resultSet destination

