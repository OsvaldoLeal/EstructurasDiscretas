import Data.Bits
import Text.Read (Lexeme(String))
main :: IO ()
main = do
    putStrLn "Bienvenidos al Himalaya!... ¿Helado? :D"

{- Descripción: Implementa la función sayHello debe recibir una cadena de texto y devolver la concatena-
ción con hello!
Uso: sayHello "Paco el flaco"
-}
sayHello :: String -> String
sayHello nombre = "Hello, " ++ nombre ++ "!"

{-Descripción: Calcula la propina que se deja en una cuenta. En este caso particular es el 15%
Uso: Fuiste con tus compas a festejar el cumpleaños de un amigo. Echaron suertes y te toco pagar la cuenta. calcularPropina 1350
-}
calcularPropina :: Float -> Float
calcularPropina x = x * 0.15


{-Descripción: Implementa la función menor que recibe 3 números y devuelve el menor de ellos.
Uso: menor (37, 45, 15)
-}
menor :: (Int, Int, Int) -> Int
menor (x, y, z) = 
    if x < y && x < z then x
    else if y < x && y < z then y
    else z


{-Descripción: La función decide recibe un booleano y dos cadenas. Si el valor booleano es True, devuelve
la primera cadena, de lo contrario devuelve la segunda.
Uso: decide True "Este es el bueno!" "Este no"
-}
decide :: Bool -> String -> String -> String
decide condicion cadena1 cadena2 = if condicion then cadena1 else cadena2


{-Descripción: La función esDescendiente recibe cuatro parámetros de tipo entero x, y, z y w. La función
debe devolver una valor de tipo booleano de acuerdo a los siguientes casos:
• True, si los números fueron ingresados de manera descendente.
• False, si los números no fueron ingresados de manera descendente
Uso: esDescendiente 56 23 9 2
-}
esDescendiente :: Int -> Int -> Int -> Int -> Bool
esDescendiente x y z w = x > y && y > z && z > w


{-Descripción: La función esDivisible debe recibir dos parámetros de tipo entero, es decir x , y y devolver
una salida del estilo x es divisible por y.
Uso: esDivisible 45 5
-}
esDivisible :: Int -> Int -> String
esDivisible x y = if y == 0 then "No es posible dividir entre cero" 
    else if mod x y == 0 then show x ++ " es divisible entre " ++ show y 
    else show x ++ " no es divisible entre " ++ show y 


{-Descripción: Implementa tu propia función esPar. Si n es par devuelve True de lo contrario False.
Uso: esPar 57
-}
esPar :: (Bits a, Num a, Eq a) => a -> Bool
esPar n = (n .&. 1) == 0


{-Descripción: La función hipotenusa debe recibir dos parámetros de tipo flotante b y h donde b representa
la base y h la altura. La función debe devolver un valor de tipo flotante que represente el
valor de la hipotenusa que se calcula respecto a la base y altura del triángulo rectángulo.
Uso: hipotenusa 24.5 12.2
-}
hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt(b^2 + h^2) 

{-Descripción: Esta función debe recibir dos parámetros que serán tuplas de dos elementos de tipo flo-
tante respectivamente, es decir, (x1, y1) y (x2, y2). pendiente debe devolver un valor de tipo
flotante que represente la pendiente de la recta que pasa por dos puntos.
Uso: pendiente (12.0, 15.4) (3.3, 12.5)

-}
pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1) 

{-Descripción: Implementa la función distanciaPuntos debe recibir dos parámetros que serán tuplas de
dos elementos de tipo flotante respectivamente, es decir, (x1, y1) y (x2, y2). La función debe
devolver un valor de tipo flotante que represente la distancia entre los puntos (x1, y1) y
(x2, y2)
Uso: distaciaPuntos (12, 34) (22, 1)
-}
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

{-Descripción: Implementa la función cuadrados que debe recibir como parámetro una tupla de tipo entera
y devolver un tupla de los cuadrados del parámetro.
Uso: cuadrados [3, 4, 7, 9]
-}
cuadrados :: [Int] -> [Int]
cuadrados xs = [x * x | x <- xs ]