
module Practica where

--Hace las operaciones--
operacion:: String -> Int -> Int -> Int
operacion "+" n1 n2 = n1 + n2
operacion "-" n1 n2 = n1 - n2
operacion "*" n1 n2 = n1 * n2
operacion "/" n1 n2 = n1 `div` n2
operacion "%" n1 n2 = n1 `mod` n2
	
--Devuelve la prioridad del operador--
eval:: String -> Int
eval "+" = 1
eval "-" = 1
eval "*" = 2
eval "/" = 2
eval "%" = 2

calcular:: [Int] -> [String] -> [Int]
calcular x [] = x
calcular (x1:x2:xs) (y1:ys) = calcular ((operacion y1 x1 x2):xs) ys
	

		

{-calcular:: [Int] -> [String] -> [Int]
calcular x y= undefined-}

tratar:: [String] -> Int
tratar lista = undefined

-- calcular [3,2,4,5] ["+","+","-"] --