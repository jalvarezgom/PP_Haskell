
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
eval "(" = 3
eval ")" = 4
eval _ = 0

calcular:: [Int] -> [String] -> Int
calcular x [] = head x
calcular (x1:x2:xs) (y1:ys) = calcular ((operacion y1 x1 x2):xs) ys


	
tratar:: String -> ([Int],[Int],[Int],[String]) -> ([Int],[Int],[Int],[String])
tratar valor (prtn,prtop,accn,accop) 
	| eval valor == 0 = (prtn,prtop,(read valor::Int):accn,accop) 
	| eval valor == 1 && (length prtop)==0  = (prtn,prtop, [calcular accn accop], [valor])
	| eval valor == 2 && (length prtop)==0  = (prtn,prtop, accn, valor:accop)
	| eval valor == 3 = ((length accn):prtn,(length accop):prtop,accn,accop)
	| eval valor == 4 = (tail prtn,tail prtop, accn,  accop)
	| (eval valor == 1 || eval valor == 2) && (head prtop)==(length accop) = (prtn,prtop,accn,valor:accop)
	-- | otherwise = (prt,accn,valor:accop)--
	
prueba:: [String] -> ([Int],[Int],[Int],[String])
prueba lista = foldl (\ (prtn,prtop,accn,accop) x -> tratar x (prtn,prtop,accn,accop) ) ([],[],[],[]) lista
		
-- 	| eval valor == 4 = (tail prtn,tail prtop, (calcular (take (length accn - head prtn) accn) (take (length accop - head prtop) accop))++(drop (length accn - head prtn) accn),  drop (length accop - head prtop) accop)--	| eval valor == 4 = (tail prtn,tail prtop, (calcular (take (length accn - head prtn) accn) (take (length accop - head prtop) accop))++(drop (length accn - head prtn) accn),  drop (length accop - head prtop) accop)



-- prueba ["2","+","3","*","2","+","1"] --
-- calcular [3,2,4,5] ["+","+","-"] --