# Huffman-Tree
An implementation of Huffman trees in Haskell. Repository copied and corrected from a group member's repository.

## Conclusión
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
