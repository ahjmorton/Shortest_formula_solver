module Model (
   Operation(..), 
   Formula(..), 
   apply, 
   evaluate, 
   formulaLength,
   addOp, 
   firstResult,
   add,
   sub,
   mult,
   signFlip,
   removeLastNumber,
   reverseNumber,
   divi,
   divf,
   addNumber,
   mapNumber,
   sumNumber
   ) where

import Data.Char (digitToInt)
import Data.List (sum)

data Operation a = 
  BinOp { sign::String, arg::a, binop::a -> a -> a } | 
   UnOp { sign::String, unop::a -> a}

instance Show a => Show (Operation a) where 
   show (BinOp sign arg _) = sign ++ " " ++ show arg
   show (UnOp sign _) = sign

data Formula a = Result a [Operation a] 

instance Show a => Show (Formula a) where
    show (Result start ops) = unwords $ (show start):(map (\op -> show op) ops)

formulaLength :: Formula a -> Int
formulaLength (Result _ ops) = length ops

apply :: a -> Operation a -> a
apply left (BinOp _ arg function) = function left arg
apply left (UnOp _ function) = function left

evaluate :: Formula a -> a
evaluate (Result start ops) = foldl apply start ops

addOp :: Formula a -> Operation a -> Formula a
addOp (Result start ops) op = (Result start (ops ++ [op]))

firstResult :: a -> Formula a
firstResult start = Result start []

add :: Num a => a -> Operation a
add value = BinOp "+" value (+) 

sub :: Num a => a -> Operation a
sub value = BinOp "-" value (-) 

mult :: Num a => a -> Operation a
mult value = BinOp "*" value (*) 

divi :: Integral a => a -> Operation a
divi value = BinOp "/" value (div) 

divf :: RealFrac a => a -> Operation a
divf value = BinOp "/" value (/) 

removeLastNumber :: Integral a => Operation a
removeLastNumber = UnOp "<<" (\value -> quot value 10)

signFlip :: Num a => Operation a
signFlip = UnOp "+/-" (negate)

doAddNumber :: Integer -> Integer -> Integer
doAddNumber left right = read $ show left ++ show right

addNumber :: Integer -> Operation Integer
addNumber value = BinOp "ins" value (doAddNumber)

doReverseNumber :: Integer -> Integer
doReverseNumber n 
  | n < 0 = 0 - (read . reverse . tail . show $ n )
  | otherwise = read . reverse . show $ n

reverseNumber :: Operation Integer
reverseNumber = UnOp "rev" (doReverseNumber)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ []    = [] 
replace from to s 
   | take (length from) s == from = to ++ (replace from to (drop (length from) s))
   | otherwise = [head s] ++ (replace from to (tail s))

doMapNumber :: Integer -> Integer -> Integer -> Integer
doMapNumber from to value = read $ replace fromStr toStr valueStr
   where 
   fromStr = show from
   toStr = show to
   valueStr = show value

mapNumber :: Integer -> Integer -> Operation Integer
mapNumber from to = UnOp (show from ++ "=>" ++ show to) (doMapNumber from to)

doSum :: Integer -> Integer
doSum value = sum $ map (toInteger . digitToInt) (show value)

sumNumber :: Operation Integer
sumNumber = UnOp "sum" (doSum)
