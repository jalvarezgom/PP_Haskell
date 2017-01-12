
module Practica1 where
import System.IO  
import System.IO.Unsafe

-- Main --
main = do
	let entrada = lecturaDatos
	let intermedio = interm entrada
	writeFile "intermedio.txt" (unlines (concatenar intermedio))
	let fin = final intermedio
	let resul_final = (concatenarResultado entrada fin)
	writeFile "final.txt" (unlines resul_final)
	return resul_final

-- Carga datos de un fichero --
cargarDatos:: IO [String]
cargarDatos = do 
	handle <- readFile "entrada.txt" 
	let expresiones = lines handle
	return expresiones

-- Adapta los datos cargados a un tipo, en este caso [String]
lecturaDatos:: [String]
lecturaDatos = unsafePerformIO cargarDatos

-- Funcion dada una [String] los une entre dejando un espacio
concatenarString:: [String] -> String
concatenarString lista = foldr (\ valor acc -> acc  ++ valor ++ " ")"" lista

-- Funcion para usar concatenarString en una lista de [String]
concatenar:: [[String]] -> [String]
concatenar lista = foldr (\ valor acc -> (concatenarString valor):acc) [] lista

-- Funcion para concatenar el resultado final y las expresiones
concatenarResultado:: [String] -> [[String]] -> [String]
concatenarResultado [] [] = []
concatenarResultado (l1:ls) ((r1:_):rs) = (l1 ++ " = " ++ r1):concatenarResultado ls rs

-- FASE 1 A PARTIR DE AQUI--
-- Funcion para evaluar prioridades fuera
evalF:: String -> Int
evalF "+" = 1
evalF "-" = 1
evalF "*" = 2
evalF "/" = 2
evalF "(" = 5
evalF ")" = 5
evalF "^" = 3
evalF _ = -1

-- Funcion para evaluar prioridades fuera
evalD:: String -> Int
evalD "+" = 1
evalD "-" = 1
evalD "*" = 2
evalD "/" = 2
evalD "(" = 0
evalD ")" = 0
evalD "^" = 4
evalD _ = -1

-- Funcion para separar la expresion en cada operador y operando
separarString:: String -> [String]
separarString x = words x

-- Funcion que se encarga en desapilar hasta encontrar un operador con menor prioridad que el
desapilar:: String -> ([String],[String]) -> ([String],[String])
desapilar valor (pstf,[])= (pstf,[valor])
desapilar valor (pstf,opd) = if (evalF valor > evalD (head opd)) then (pstf,valor:opd) else desapilar valor ((head opd):pstf , tail opd)

-- Funcion para desapilar en caso de encontrar un ")"
desapilarprt:: ([String],[String]) -> ([String],[String])
desapilarprt (pstf,[])= (pstf,[])
desapilarprt (pstf,opd) = if ((head opd)=="(") then (pstf, tail opd) else desapilarprt ((head opd):pstf , tail opd)

-- Funcion para tratar los valores y distinguir los distintos casos con operadores y numeros
tratar:: String -> ([String],[String]) -> ([String],[String])
tratar valor (pstf,opd)
	| evalF valor == -1 = (valor:pstf,opd) -- Valor es un numero 
	| valor == ")" = desapilarprt (pstf,opd) -- Valor es )
	| (length opd) == 0 = (pstf,valor:opd) -- Si la lista de operadores esta vacia se inserta
	| evalF valor > evalD (head opd) = (pstf,valor:opd) -- Valor es un operador con mayor prioridad
	| otherwise = desapilar valor (pstf,opd)

-- Funcion para tratar una funcion ya separada
prueba:: [String] -> ([String],[String])
prueba lista = foldl (\ (postfija,opd) x -> tratar x (postfija,opd) ) ([],[]) lista

-- Funcion que adapta el resultado devolviendo una unica lista de string y añadiendo la lista de operadores a la postfija
adaptar:: ([String],[String]) -> [String]
adaptar (pstf,opd) = (reverse opd) ++ pstf

-- Funcion que convierte una expresion a Postfija
fase1:: String -> [String]
fase1 x = adaptar (prueba (separarString x))

-- Funcion que resuelve la FASE 1, que dado una lista de expresiones las convierte a Postfija
interm:: [String] -> [[String]]
interm lista = init (foldr (\x acc -> (fase1 x):acc)[[]] lista)

-- FASE 2 A PARTIR DE AQUI --
-- Funcion para hacer las distintas posible operaciones
operacion:: String -> String -> String -> Int
operacion "+" n1 n2 = (read n1::Int) + (read n2::Int)
operacion "-" n1 n2 = (read n1::Int) - (read n2::Int)
operacion "*" n1 n2 = (read n1::Int) * (read n2::Int)
operacion "/" n1 n2 = (read n1::Int) `div` (read n2::Int)
operacion "^" n1 n2 = elevado (read n1::Int) (read n2::Int) (read n1::Int)

--Funcion que hace la operacion ^
elevado:: Int -> Int -> Int -> Int
elevado _ 1 z = z
elevado x y z = elevado x (y-1) (z*x) 

-- Funcion que calcula el resultado de una expresion Postfija
resultado:: [String] -> [String]
resultado lista = foldr (\ valor acc -> if (evalF valor == -1) then valor:acc else calcular valor acc )[] lista

-- Funcion que se encarga de calcular el resultado e insertarlo en la pila de numeros
calcular:: String -> [String] -> [String]
calcular opd (x1:x2:xs) = (show (operacion opd x2 x1)):xs

-- Funcion que aglomera las distintas funciones para obtener el resultado de una expresion
fase2:: String -> [String]
fase2 x = resultado (fase1 x)

-- Funcion que resuelve la FASE 2, dado una lista formada por listas de cada expresin convertidas a Postfija
final:: [[String]] -> [[String]]
final lista = foldr (\ valor acc -> (resultado valor):acc)[] lista
 