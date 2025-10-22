module Huffman where

import Data.Map as DM (Map, fromList, insertWith, empty, toList, insert, lookup)
import Heap

{-

Integrantes:

- Ernesto Savio
- Santiago Bussanich
- Valentin Sosa

-}

-- Bits y códigos

data Bit = Zero | One deriving (Eq, Show)

type Code = [Bit]

-- Árbol de codificación

data HTree = Leaf Char Int
           | Node HTree HTree Int
           deriving Show

weight :: HTree -> Int
weight (Leaf _ w)   = w
weight (Node _ _ w) = w

-- Diccionarios de frecuencias y códigos

type FreqMap = Map Char Int

type CodeMap = Map Char Code


-- Ejercicio 1

-- Instancia de Eq para HTree
instance Eq HTree where
    (==) :: HTree -> HTree -> Bool
    ht1 == ht2 = weight(ht1) == weight(ht2)

-- Instancia de Ord para HTree
instance Ord HTree where
    (<) :: HTree -> HTree -> Bool
    ht1 < ht2 = weight(ht1) < weight(ht2)

    (>) :: HTree -> HTree -> Bool
    ht1 > ht2 = weight(ht1) > weight(ht2)

    (<=) :: HTree -> HTree -> Bool
    h1 <= h2 = (h1 < h2) || (h1 == h2)

    (>=) :: HTree -> HTree -> Bool
    h1 >= h2 = (h1 > h2) || (h1 == h2)

    max :: HTree -> HTree -> HTree 
    max h1 h2 = if h1 >= h2 then h1 else h2

    min :: HTree -> HTree -> HTree 
    min h1 h2 = if h1 >= h2 then h2 else h1


-- Ejercicio 2

buildFreqMap :: String -> FreqMap
buildFreqMap [] = DM.empty
buildFreqMap (x : xs) = insertWith (\ nv -> (\ ov -> nv + ov)) 
                                   x 1 (buildFreqMap xs)


-- Ejercicio 3
buildHeap :: [(Char, Int)] -> Heap HTree -> Heap HTree
buildHeap [] h = h
buildHeap ((c, w) : hs) h = buildHeap hs (Heap.insert (Leaf c w) h)

mergeHTree :: HTree -> HTree -> HTree
mergeHTree a b = Node a b (weight a + weight b)


heapToHTree :: Heap HTree -> HTree
heapToHTree heap = heapToHTree' heap []
  where
    heapToHTree' :: Heap HTree -> [HTree] -> HTree
    heapToHTree' heap hs 
                         | (isEmpty heap) = 
        case hs of
          [] -> error "No se puede construir un árbol vacío"
          (ht : [])  -> ht
          (h1 : h2 : hss) -> mergeHTree h1 h2

                         | otherwise =
        case hs of
          (h1 : h2 : hss) -> heapToHTree' (Heap.insert (mergeHTree h1 h2) heap)
                                          hss 
          _ -> heapToHTree' (deleteMin heap) ((findMin heap) : hs)
                      
    
buildHTree :: FreqMap -> HTree
buildHTree fm | null fm = error "No se puede construir un árbol vacío"
              | otherwise = heapToHTree (buildHeap (toList fm) Heap.empty)


-- Ejercicio 4

buildCodeMap :: HTree -> CodeMap
buildCodeMap ht = buildCodeMap' (getCodes ht []) DM.empty
  where
    buildCodeMap' :: [(Char, Code)] -> CodeMap -> CodeMap
    buildCodeMap' [] cm = cm
    buildCodeMap' ((c, code) : xs) cm = buildCodeMap' xs (DM.insert c code cm)

getCodes :: HTree -> Code -> [(Char, Code)]
getCodes (Leaf c _) code = [(c, reverse code)]
getCodes (Node left right _) code = getCodes left ([Zero] ++ code) ++ 
                                    getCodes right ([One] ++ code)


-- Ejercicio 5

encode :: CodeMap -> String -> Code
encode cm [] = []
encode cm (x : xs) = case (DM.lookup x cm) of
    Nothing -> error "El caracter no se encuentra en el mapa de códigos"
    Just code -> code ++ encode cm xs

-- Ejercicio 6

decode :: HTree -> Code -> String
decode _ [] = []
decode ht code = decode' ht code
                   where
                     decode' (Leaf c _) [] = [c]
                     decode' (Leaf c _) code = c : decode' ht code
                     decode' (Node l r _) (Zero : cs) = decode' l cs
                     decode' (Node l r _) (One : cs) = decode' r cs
                     decode' _ _ = error "Código inválido"

{-
 -
- Ejemplo

let fm = buildFreqMap "la luna llena"
let ht = buildHTree fm
let cm = buildCodeMap ht
let code = encode cm "la luna llena"
let decoded = decode ht code

-}


-- Ejercicio 7

engFM :: FreqMap
engFM = fromList [
    ('a', 691),
    ('b', 126),
    ('c', 235),
    ('d', 360),
    ('e', 1074),
    ('f', 188),
    ('g', 170),
    ('h', 515),
    ('i', 589),
    ('j', 13),
    ('k', 65),
    ('l', 340),
    ('m', 203),
    ('n', 571),
    ('o', 635),
    ('p', 163),
    ('q', 8),
    ('r', 506),
    ('s', 535),
    ('t', 766),
    ('u', 233),
    ('v', 83),
    ('w', 200),
    ('x', 13),
    ('y', 167),
    ('z', 6),
    (' ', 1370),
    (',', 84),
    ('.', 89)
    ]

-- Árbol de codificación de engFM
f :: HTree
f = buildHTree engFM

codemapej = buildCodeMap f

-- Strings de ejemplo: 

-- Frase de El Padrino
ej1 = "bonasera, bonasera, what have i ever done to make you treat me so disrespectfully. if you would come to me in friendship, this scum who ruined your daughter would be suffering this very day. and if by some chance an honest man like yourself made enemies they would become my enemies. and then, they would fear you." 

-- Canciones de los Beatles (acomodadas para que respeten el formato del FreqMap)
ej2 = "well, shake it up, baby, now shake it up baby, twist and shout twist and shout, come on, come on, come, come on, baby, now come on baby come on and work it on out work it on out."
ej3 = "images of broken light, which dance before me like a million eyes, they call me on and on across the universe, thoughts meander like a restless wind inside a letter box, they tumble blindly as they make their way across the universe"
ej4 = "jai guru deva, om. nothing is gonna change my world, nothing is gonna change my world, nothing is gonna change my world, nothing is gonna change my world."

-- Telegramas militares reales emitidos por el gobierno britanico en 1939-1945.
ej5 = "general mobilization has been ordered. first day of mobilization, september two."
ej6 = "biscuit to baker, the eagles have landed at zero seven zero zero. proceed with operation tango."

-- Trabalenguas 
ej7 = "how much wood would a woodchuck chuck if a woodchuck could chuck wood"

-- Fragmento de A Tale Of Two Cities, de Charles Dickens.
ej8 = "it was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of light, it was the season of darkness, it was the spring of hope, it was the winter of despair..."

-- Frase de Breaking Bad
ej9 = "you clearly do not know who you are talking to, so let me clue you in. i am not in danger, skyler. i am the danger. a guy opens his door and gets shot, and you think that of me, no. i am the one who knocks."

-- Cancion de The Doors.
ej10 = "people are strange, when you are a stranger, faces look ugly, when you are alone. women seem wicked, when you are unwanted, streets are uneven, when you are down."

-- Ejemplo con un solo caracter (el que mayor frecuencia en codemapej).
ej11 = "                                                                "

-- Ejemplo con un solo caracter (el de menor frecuencia en codemapej).
ej12 = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"


-- Para ver el codigo encriptado/desencriptado: 
-- encode codemapej ej1
-- decode f (encode codemapej ej1)

-- Devuelve el largo de la codificacion del string con el codemap pasados.
lengthEncode :: CodeMap -> String -> Int
lengthEncode cm "" = 0
lengthEncode cm (s : ss) = length (encode cm [s]) + lengthEncode cm ss

-- Calcula el largo de la codificacion fija de 5 bits por caracter.
lengthFiveBits :: String -> Int
lengthFiveBits s = 5 * length s

-- Compara los largos de codificación variable y fija.
compareLength :: String -> CodeMap -> (Int, Int)
compareLength s cm = (lengthEncode cm s, lengthFiveBits s)

-- Calcula el porcentaje de ahorro entre dos largos de codificación.
percent :: (Int, Int) -> (Char, Float)
percent (huf, bit)
    | huf < bit = ('H', 100 - (\x y -> (fromIntegral x * 100) / fromIntegral y) huf bit)
    | otherwise = ('B', 100 - (\x y -> (fromIntegral x * 100) / fromIntegral y) bit huf)

-- Aplica la función percent a una lista de pares de largos.
applyPercent :: [(Int, Int)] -> [(Char, Float)]
applyPercent [] = []
applyPercent (x : xs) = percent x : applyPercent xs

-- Todos los largos agrupados en una lista.
ejlen = [
    compareLength ej1 codemapej,
    compareLength ej2 codemapej,
    compareLength ej3 codemapej,
    compareLength ej4 codemapej,
    compareLength ej5 codemapej,
    compareLength ej6 codemapej,
    compareLength ej7 codemapej,
    compareLength ej8 codemapej,
    compareLength ej9 codemapej,
    compareLength ej10 codemapej, 
    compareLength ej11 codemapej, 
    compareLength ej12 codemapej]

-- Todos los porcentajes agrupados en una lista.
ejper = applyPercent ejlen

{-

Conclusion: 

La codificación de Huffman con longitudes variables resulta ser significativamente más eficiente en términos 
de espacio en comparación con la codificación fija de 5 bits por carácter. Esto se debe a que Huffman asigna 
códigos más cortos a los caracteres más frecuentes y códigos más largos a los menos frecuentes, optimizando 
el uso del espacio. En los ejemplos analizados, se observa una reducción considerable en el tamaño de la 
codificación en la mayoria de casos, alcanzando porcentajes de ahorro que varían dependiendo de la distribución 
de frecuencias de los caracteres en cada texto. Esto indica que si el texto contiene en su mayoria caracteres con 
mayor frecuencia, su codificacion sera mas corta que la de 5 bits. Una prueba cruda de esto se ve en los ultimos 2 
test donde se comparan las longitudes de ambos encriptados para el caracter mas frecuente y el caracter menos frecuente
del codemapej. En conclusion la eleccion de un tipo de encriptado va a depender del texto en particular, pero en casos
generales, el de Huffman es un ~15% debido a que las frecuencias de caracteres estan diseñadas para que el encriptado sea 
mas eficiente y corto.

-}
