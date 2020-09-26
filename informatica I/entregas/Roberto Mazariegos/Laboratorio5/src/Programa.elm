module Programa exposing (..)

--ejercicio 1
existe xs sb =

    case xs of 
    bs:: b1  ->
        if  bs == sb then
         True 
     else 
          existe  b1 sb
    [] -> False 

--ejercicio 2 

mayoresQue  xs sb   = mayorAux xs sb []

mayorAux xs sb l =
    case xs of 
    [] -> l
    b :: bs -> 
            if sb < b 
            then b :: mayorAux bs sb l
            else mayorAux bs sb l


-- Ejercicio #3
promedio x =
    let
        y =
            List.length x

        z =
            List.sum x 
    in
     z / toFloat y

-- Ejercicio #4
type Naturales = Cons Naturales
    | Cero 

normalNatural xs =
    case xs of
    0 -> Cero
    b -> Cons (normalNatural (b - 1) )

igual a b = 
    case (a , b) of
    (Cons xl, Cons vb) -> igual xl vb
    (Cero, Cero) -> True
    _ -> False


-- Ejercicio #5
mayorQueNaturales a b =
    case (a, b) of
    (Cons xl, Cons vb) -> mayorQueNaturales xl vb
    (Cons xl, Cero) -> True
    (Cero, _) -> False

-- Ejercicio #6

mayorNaturales xs y =
    mayorNaturalesAux xs y []


mayorNaturalesAux xs a l =
    case xs of
    [] -> l
    b :: bs ->
        if mayorQueNaturales b a
        then b :: mayorNaturalesAux bs a l
        else mayorNaturalesAux bs a l





