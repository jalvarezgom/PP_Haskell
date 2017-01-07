
module Practica1 where
import Data.List

operacion:: String -> Int -> Int -> Int
operacion "+" n1 n2 = n1 + n2
operacion "-" n1 n2 = n1 - n2
operacion "*" n1 n2 = n1 * n2
operacion "/" n1 n2 = n1 `div` n2
operacion "^" n1 n2 = undefined

evalF:: String -> Int
evalF "+" = 1
evalF "-" = 1
evalF "*" = 2
evalF "/" = 2
evalF "(" = 5
evalF ")" = 5
evalF "^" = 3
evalF _ = -1

evalD:: String -> Int
evalD "+" = 1
evalD "-" = 1
evalD "*" = 2
evalD "/" = 2
evalD "(" = 0
evalD ")" = 0
evalD "^" = 4
evalD _ = -1

separarString:: String -> [String]
separarString x = words x

desapilar:: String -> ([String],[String]) -> ([String],[String])
desapilar valor (pstf,[])= (pstf,[valor])
desapilar valor (pstf,opd) = if (evalF valor > evalD (head opd)) then (pstf,valor:opd) else desapilar valor ((head opd):pstf , tail opd)

desapilarprt:: ([String],[String]) -> ([String],[String])
desapilarprt (pstf,[])= (pstf,[])
desapilarprt (pstf,opd) = if ((head opd)=="(") then (pstf, tail opd) else desapilarprt ((head opd):pstf , tail opd)

tratar:: String -> ([String],[String]) -> ([String],[String])
tratar valor (pstf,opd)
	| evalF valor == -1 = (valor:pstf,opd) -- Valor es un numero 
	| valor == ")" = desapilarprt (pstf,opd) -- Valor es )
	| (length opd) == 0 = (pstf,valor:opd)
	| evalF valor > evalD (head opd) = (pstf,valor:opd) -- Valor es un operador con mayor prioridad
	| otherwise = desapilar valor (pstf,opd)

prueba:: [String] -> ([String],[String])
prueba lista = foldl (\ (postfija,opd) x -> tratar x (postfija,opd) ) ([],[]) lista

adaptar:: ([String],[String]) -> [String]
adaptar (pstf,opd) = (head opd):pstf

main:: String -> [String]
main x = adaptar (prueba (separarString x))
