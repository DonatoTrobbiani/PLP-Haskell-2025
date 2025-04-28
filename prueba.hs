import Data.List (sort)

main :: IO ()
main = do
  print (piAproxNewton 5)

{-  2. Dar una definicion de la funcion losCuatrosIguales con el siguiente tipo:
Int -> Int -> Int -> Int -> Bool
la cual da como resultado True si sus cuatros argumentos son iguales. -}
losCuatroIguales :: Int -> Int -> Int -> Int -> Bool
losCuatroIguales a b c d = (a==b) && (b==c) && (c==d)

--  3. ¿Puede dar una definici´on de losCuatroIguales usando la funci´on allEqual?

{-  4. Escriba definiciones de funciones para:
cuantosIguales:: Int -> Int -> Int -> Int
cuantosIgualesDeDos :: Int -> Int -> Int
las cuales cuentan si sus argumentos son iguales.  -}
cuantosIguales :: Int -> Int -> Int -> Int
cuantosIguales a b c
  | a==b && b==c = 3
  | a==b || b==c || a==c = 2
  | otherwise = 0

cuantosIgualesDeDos :: Int -> Int -> Int
cuantosIgualesDeDos a b
  | a == b = 2
  | otherwise = 0

--5. Definir las siguientes funciones:
--(a) Que devuelva el enesimo elemento de la serie de Fibonacci.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--(b) El factorial de un numero n
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--(c) El valor de la siguiente sucesion:
f :: Int -> Int -> Float
f x n = sum [fromIntegral (i^n) | i <- [1..n]] / fromIntegral (factorial x)

{-   6. Dar una definicion para la funcion:
nAnd :: Bool -> Bool -> Bool
que devuelve True excepto cuando sus dos argumentos son ambos True  -}
nAnd :: Bool -> Bool -> Bool
nAnd x y
  | x && y = False
  | otherwise = True

{- 7. ¿Como se podrıa simplificar esta definicion de forma que quede una unica clausula?:
funny x y z
| x > z = True
| x >= y = False
| otherwise = True -}
funny :: Int -> Int -> Int -> Bool
funny x y z = x < y

{- 8. Dar una definicion de la funcion:
allDiferent :: Int -> Int -> Int -> Bool
se podrıa utilizar la funcion “/=” con la propiedad que m /= n es True si m y n no son iguales. Probar la funcion allDiferent con diferentes valores. -}
allDiferent :: Int -> Int -> Int -> Bool
allDiferent x y z = (x /= y) && (y /= z) && (x /= z)

{- 10. Dar la definicion de la funcion:
cuartaPotencia :: Int -> Int
Dar otra definicion que use alCuadrado. -}
cuartaPotencia :: Int -> Int
cuartaPotencia x = x ^ 4

{- 
cuartaPotencia :: Int -> Int
cuartaPotencia x = alCuadrado (alCuadrado x)
-}

alCuadrado :: Int -> Int
alCuadrado x = x * x

--12. Definir una funcion tal que dado un Char que contenga un dıgito, devuelva el valor numerico de dicho dıgito.
valorChar :: Char -> Int
valorChar x = case x of
  '0' -> 0
  '1' -> 1
  '2' -> 2
  '3' -> 3
  '4' -> 4
  '5' -> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9' -> 9
  _ -> error "no es digito"

{- 13. Defina una funcion
digitoRomano :: Char -> String
la cual convierte un dıgito a su representacion en numeros romanos -}
digitoRomano :: Char -> String
digitoRomano x = case y of
  0 -> "0"
  1 -> "I"
  2 -> "II"
  3 -> "III"
  4 -> "IV"
  5 -> "V"
  6 -> "VI"
  7 -> "VII"
  8 -> "VIII"
  9 -> "IX"
  _ -> error "no es digito"
  where y = valorChar x

{- 14. Defina una funcion
entreLıneas :: String -> String -> String -> String
la cual tome tres Strings y retorne un String que cuando se imprima muestre los tres Strings en lıneas separadas. -}
entreLineas :: String -> String -> String -> String
entreLineas x y z = x ++ "\n" ++ y ++ "\n" ++ z
--entreLineas x y z = unlines[x,y,z]

{- 15. Defina una funcion
duplicar :: String -> Int -> String
la cual tome un String y un numero natural n. El resultado son n copias de un String
concatenado. (Si n=0 debe devolver un String vacıo). -}
duplicar :: String -> Int -> String
duplicar _ 0 = ""
duplicar a x = a ++ duplicar a (x-1)

{- 16. Defina una funcion
hacerEspacios :: Int -> String
tal que hacerEspacios n devuelva un String de n espacios. -}
hacerEspacios :: Int -> String
hacerEspacios x = replicate x ' '

{- 17. Dar una definicion para la funcion
factorialTable ::Int -> Int -> String
de forma que factorialTable m n tabule los valores de los factoriales desde m hasta n inclusive. Validar los datos de entrada. -}
factorialTable :: Int -> Int -> String
factorialTable m n
  | m > n = "m tiene que ser menor a n"
  | m < 0 || n < 0 = "tiene que ser numeros naturales"
  | otherwise = unlines [show x ++ " -> " ++ show (factorial x) | x <- [m..n]]

{- 18. Definir una funcion
justificarCentro :: Int -> String -> String
de forma que justificarCentro n st nos devuelva un String de longitud n en el cual sele han agregado espacios en ambos extremos de st de modo que quede centrado.
Validar todos los casos. Dar una solucion utilizando la clausula where y otra sin ella. -}
justificarCentro :: Int -> String -> String
justificarCentro n st
  | length st > n = "el string tiene que ser menor a n"
  | length st == n = st
  | otherwise = espIzq ++ st ++ espDer
  where
  totalEsp = n - length st
  espIzq = replicate (totalEsp `div` 2) ' '
  espDer = replicate (totalEsp - totalEsp `div` 2) ' '

{-
justificarCentro n st
  | length st > n = "el string tiene que ser menor a n"
  | length st == n = st
  | otherwise = replicate ((n - length st) `div` 2) ' ' ++ st ++ replicate ((n - length st) - totalEsp `div` 2) ' '
-}

--19. Definir una funcion minMax la cual retorne el minimo y el maximo de una tupla.
minMax :: [Int] -> (Int, Int)
minMax x = (minimum x, maximum x)

{- 20. Dar una definicion de la funcion
maxOcurr :: int -> Int -> (Int,Int)
el cual retorna el maximo de dos numeros, junto con el numero de veces que aparece.
Usando esta u otra funcion defina
maxOcurr :: int -> Int -> Int -> (Int,Int)
que haga lo mismo pero con tres argumentos. -}
maxOcurr :: Int -> Int -> Int -> (Int, Int)
maxOcurr x y z =
    let m = maximum [x, y, z]
    in (m, length (filter (== m) [x, y, z]))

{- 21. Dar una definicion para la siguiente funcion
ordenTriple :: (Int,Int,Int) -> (Int, Int,Int)
la cual ordena los tres elementos en orden ascendente. -}
ordenTriple :: (Int,Int,Int) -> (Int, Int,Int)
ordenTriple (a, b, c) =
    let [x, y, z] = sort [a, b, c]
    in (x, y, z)

--PARCIAL 2024
tParcial :: (Int, Int) -> Int -> String
tParcial (n, m) c
  | n < 1 || c <= 0 = error "ERROR"
  | n > m = ""
  | otherwise = unlines [show n ++ " -> " ++ show (laSumatoria (n, c))] ++ tParcial (n+1, m) c

laSumatoria :: (Int, Int) -> Float
laSumatoria (k, c) = sum [fromIntegral (x^3 + (5*c)) / fromIntegral (x*2 + c - 1)| x <- [1..k]]

--RECU 2024
piAproxNewton :: Int -> (Float, Float)
piAproxNewton n = (sumatoriaPi n, pi - sumatoriaPi n)

sumatoriaPi :: Int -> Float
sumatoriaPi n = ((3 * (3 ** (1/2))) / 4) + 2 - ((3/4) * sum [ binomio (2*j, j) / fromIntegral ((j+1)*(2*j + 5) * 16^j) | j <- [0..n]])

binomio :: (Int, Int) -> Float
binomio (p, r) = fromIntegral (factorial p) / fromIntegral (factorial r) * fromIntegral (factorial (p - r))