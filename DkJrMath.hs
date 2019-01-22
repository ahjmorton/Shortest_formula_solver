module DkJrMath (solutions) where

import Model

ops :: Integral a => [a -> Operation a]
ops = [mult, add, divi, sub]

universe :: Integral a => [a]
universe = [1..10]

availableOps :: Integral a => [Operation a] 
availableOps = [op item | op <- ops, item <- universe]

createNextSolutions :: Integral a => a -> Formula a -> [Formula a]
createNextSolutions destination form =
    filter isCloser $ map (addOp form) availableOps
    where 
    distance value = abs $ destination - value
    currentResult = evaluate form
    currentDistance = distance currentResult
    isCloser formula = distance (evaluate formula) <  currentDistance

createPossibleSolutions :: Integral a => [Formula a] -> a -> [Formula a]
createPossibleSolutions resultSet destination = 
    possibleSolutions ++ createPossibleSolutions possibleSolutions destination
    where 
    possibleSolutions = concatMap (createNextSolutions destination) resultSet

solutions :: Integral a => a -> a -> [Formula a]
solutions start destination = createPossibleSolutions [(firstResult start)] destination
