import Model 
import qualified DkJrMath 

isShortest :: Integral a =>  a -> Formula a -> Bool
isShortest destination formula = evaluate formula == destination

findShortest :: Integral a => [Formula a] -> a -> Formula a
findShortest resultSet destination = 
    head $ filter (isShortest destination) resultSet
