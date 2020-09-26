module Programa exposing (..)

type ArbolBinario = Hoja | Rama Int ArbolBinario ArbolBinario

arbol = Rama 
--ejercicio 1

contar x = 
    case x of
    Rama b bs s -> (1) + contar bs + contar s
    Hoja -> 0 
--ejercico 2

revisar x n =
    case x of 
    Rama b bs s -> 
        if n == b 
        then True
        else revisar bs n || revisar s n
    Hoja -> False

--ejercicio 3


incrementar x =
    case x of
    Rama b bs s -> Rama (b+1) (incrementar (bs)) (incrementar (s)) 
    Hoja -> Hoja

--ejercicio 4

type ArbolB = HojaB | RamaB Int (List ArbolB)

-- ejercicio 5

incrementarB x = 
    case x of
    RamaB b bs -> RamaB (b+1) (List.map incrementarB bs)   
    HojaB -> HojaB 