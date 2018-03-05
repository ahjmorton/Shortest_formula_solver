data Op = Sub | Mult | Div | Add deriving Enum

instance Show Op where 
   show Sub = "-"
   show Mult = "x"
   show Div = "/"
   show Add = "+"

data Formula a = Result a [(Op, a)] 

instance Show a => Show (Formula a) where
    show (Result start ops) = unwords $ (show start):(map (\op -> show (fst op) ++ " " ++ show (snd op)) ops)

operation :: Integral a => Op -> (a -> a -> a)
operation Sub = (-)
operation Mult = (*)
operation Div = (div)
operation Add = (+)

performOp :: Integral a => a -> (Op, a) -> a
performOp left (op, right) = (operation $ op) left right

evaluate :: Integral a => Formula a -> a
evaluate (Result start ops) = foldl performOp start ops

addOp :: Integral a => Formula a -> (Op, a) -> Formula a
addOp (Result start ops) op = (Result start (ops ++ [op]))

createNextOps :: Integral a => [a] -> a -> Formula a -> [Formula a]
createNextOps universe destination form =
    map (addOp form) newOps
    where 
    distance value = abs $ destination - value
    currentResult = evaluate form
    currentDistance = distance currentResult
    newOps = [(op, item) | op <- [Sub, Add, Mult, Div], item <- universe, distance(performOp currentResult (op, item)) < currentDistance]

createNextGeneration :: Integral a => [Formula a] -> [a] -> a -> [Formula a]
createNextGeneration resultSet universe destination = 
    concatMap (createNextOps universe destination) resultSet
       

hasShortest :: Integral a =>  [Formula a] -> a -> Maybe (Formula a)
hasShortest [] _ = Nothing
hasShortest (x:xs) destination 
     | evaluate x == destination = Just x
     | otherwise = hasShortest xs destination

doFindShortest :: Integral a => [Formula a] -> a -> [a] -> Formula a
doFindShortest resultSet destination universe = 
    case (hasShortest resultSet destination) of
        (Just result) -> result
        _ -> doFindShortest (createNextGeneration resultSet universe destination) destination universe
    
findShortest :: Integral a => a -> a -> [a] -> Formula a
findShortest start destination universe = doFindShortest [(Result start [])] destination universe

findShortestFromZero :: Integral a => a -> [a] -> Formula a
findShortestFromZero destination universe = findShortest 0 destination universe
