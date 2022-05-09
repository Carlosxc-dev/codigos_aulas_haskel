import Data.List -- para fun¸c~ao sort

type ConjuntoInt = [Int]

pertence :: Int -> ConjuntoInt -> Bool
pertence _ [] = False
pertence e (a:cauda)    
    | (e == a) = True
    | otherwise = pertence e cauda

cardinalidade :: ConjuntoInt -> Int
cardinalidade [] = 0
cardinalidade (_:cauda) = 1 + cardinalidade cauda

subConjunto :: ConjuntoInt -> ConjuntoInt -> Bool
subConjunto conj1 conj2 = subConjuntoAux conj1 conj2

subConjuntoAux :: ConjuntoInt -> ConjuntoInt -> Bool
subConjuntoAux [] _ = True
subConjuntoAux (a:cauda1) b 
    | not (pertence a b) = False
    | otherwise = subConjuntoAux cauda1 b

comparaveis :: ConjuntoInt -> ConjuntoInt -> Bool
comparaveis a b = subConjunto a b || subConjunto b a

iguais :: ConjuntoInt -> ConjuntoInt -> Bool
iguais a b = subConjunto a b && subConjunto b a

uniao :: ConjuntoInt -> ConjuntoInt -> ConjuntoInt
uniao conj1 conj2 = retiraRepetidos (conj1 ++ conj2)

retiraRepetidos :: ConjuntoInt -> ConjuntoInt
retiraRepetidos [] = []
retiraRepetidos (a:cauda1) = a : retiraRepetidos (retiraTodos a cauda1)

retiraTodos :: Int -> ConjuntoInt -> ConjuntoInt
retiraTodos _ [] = []
retiraTodos x (a:cauda1) 
    | (x == a) = retiraTodos x cauda1
    | otherwise = a : retiraTodos x cauda1

inter :: ConjuntoInt -> ConjuntoInt -> ConjuntoInt
inter [] _ = []
inter (a:cauda1) conj2 | pertence a conj2 = a : inter cauda1 conj2
    | otherwise = inter cauda1 conj2

diferenca :: ConjuntoInt -> ConjuntoInt -> ConjuntoInt
diferenca a [] = a
diferenca conj1 (a:cauda2) = diferenca (retiraTodos a conj1) cauda2

potencia :: ConjuntoInt -> [ConjuntoInt]
potencia [] = [[]]
potencia (a:b) = potencia b ++ [(a:z) | z <- potencia b]

--diferenca saimetrica (A-B)U(B-A)
difSim::ConjuntoInt -> ConjuntoInt -> ConjuntoInt
difSim a b = (diferenca a b)  ++ (diferenca b a)

fsa :: Int -> Int
fsa x
    |x==0 = 1
    |x==1 = 3
    |otherwise = 3*fsa(x-1)

fsb :: Int -> Float
fsb x = 2 / 2^x

fsc::Int -> Int
fsc x 
    | x == 0 = 1
    | otherwise = fsc(x-1) + x