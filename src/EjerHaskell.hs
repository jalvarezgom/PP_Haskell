
module EjerHaskell where
import Data.Char

orden::Int->Int->Int->Bool
orden x y z =(z>y) && (y>x)

pagar::Int->Int
pagar x
	| x<=18 = 20
	| x>18 && x<=35 =50
	| x>35 && x<=65 =40
	| x>65 = 20
	
isMayuscula:: String->String
isMayuscula palabra = [char| char<-palabra, isUpper char]

contarletra::String->Char->Int
contarletra palabra aux= length [char| char<-palabra, char==aux]

esPrimo::Int->Bool
esPrimo num = length [x|x<-[1..num], num `mod` x ==0] ==2

tupla::((String,Int),(String,Int),(String,Int))->(String,String,String)
tupla ((s1,_),(s2,_),(s3,_)) = (s1,s2,s3)

cribar1:: [Int]->Int->[Int]
cribar1 lista x= [ y | y <- lista, not (y `rem` x ==0)]

cribar2:: [Int]->Int->[Int]
cribar2 [] _ = []
cribar2 (x:xs) n = if x `mod` n == 0 then cribar2 xs n else x:(cribar2 xs n)

pertenece:: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece n (x:xs)= (n==x) || (pertenece n xs)

aux:: [Int] -> Int -> [Int] -> [(Int,Int)] -> [(Int,Int)]
aux (x:xs) posicion repeticiones acumulador =  if pertenece x repeticiones then aux xs (posicion+1) repeticiones acumulador
											else aux xs (posicion+1) (x:repeticiones) (acumulador++[(x,posicion)])
											
duplas:: [Int] -> [(Int,Int)]
duplas l = aux l 1 [] []

secuencias:: [Int]->Int
secuencias [] = 0
secuencias [x]= if x==0 then 1 else 0
secuencias (x:y:ys) = if x==0 then
						if y /= 0 then (1+secuencias ys)
						else secuencias (y:ys)
					else secuencias (y:ys)
					
insertFinal:: Int->[Int]->[Int]
insertFinal n l =foldr(\ x y -> x:y) [n] l

eliminar:: Int->[Int]->[Int]
eliminar _  []=[]
eliminar n lista=foldr(\ x ys -> if x==n then ys else (x:ys))[] lista
										
cambiarMayus:: [Char]->[Char]
cambiarMayus l = foldr(\ x xs -> if not(isUpper x) then (toUpper x):xs else (toLower x):xs)[] l

listacambiarMayus:: [String]->[String]
listacambiarMayus l = map(cambiarMayus) l	

separar::[Int]->([Int],[Int])
separar l = foldr (\ e (pares,impares) -> if (e `mod` 2 ==0) then (e:pares , impares) else (pares , e:impares)) ([],[]) l	

cambio:: [Int]->Int->Int->[Int]
cambio l a b = x where (x,y) = foldl (\ (acum,pos) e-> if (e==a)&&(odd pos) then (acum++[b],pos+1) else (acum++[e],pos+1)  ) ([],1) l

data Fraccion= F Int Int deriving Show
equivalentes:: [Fraccion] -> Fraccion -> [Fraccion]
equivalentes l (F num den) = foldr (\(F n d)acum -> if n*den==num*d then (F n d):acum else acum )[] l									

data Arbol a = AV|Nodo a (Arbol a) (Arbol a) deriving Show

--Ejercicio 1 Restaurante--
type NMesa = Int
type Capacidad = Int
data Mesa = M NMesa Capacidad
instance Show Mesa where
	show (M n c) = "Mesa " ++show n++" Capacidad: "++show c

	
type Libres = [Mesa]
type Ocupadas = [Mesa]
data Restaurante = R Libres Ocupadas
instance Show Restaurante where
	show (R l o) = "Libres: " ++ show l++"\nOcupadas: " ++show o

--Quicksort--
{-
qs:: [Ord a]->[a]->[a]
qs [] = []
qs (x:xs) = qs [z | z<-xs, z<=x]  ++ [x] ++ qs [z | z<-xs, z>x] -}
