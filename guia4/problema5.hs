{--
Ejercicio 5. Implementar la funci´on medioFact :: Integer->Integer que dado n ∈ N calcula n!! = n(n−2)(n−4)···.
 problema medioFact (n: Z) : Z {
 requiere: { n ≥ 0 }
 ⌊n−1
 2 ⌋
 asegura: { resultado =
 }
 Por ejemplo:
 medioFact 10 
i=0
 (n −2i) }
 10 ∗8∗6∗4∗2 3840.
 medioFact 9 
medioFact 0 
9 ∗7∗5∗3∗1 945.
 1.
--}

medioFact :: Integer -> Integer
medioFact n | n == 0 = 1
            | n == 1 = 1
            | n-2 >= 0 = n * medioFact (n-2)