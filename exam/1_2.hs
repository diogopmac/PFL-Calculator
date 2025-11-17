import Data.Char (chr, ord)
type Letras = (Char, Char)
type Digitos = (Int, Int)
type Matricula = (Letras, Digitos, Letras)

incrMatricula :: Matricula -> Matricula
incrMatricula (l1, d, l2)
    | (incrDigitos d == (0,0)) && (incrLetras l2 == ('A', 'A')) = (incrLetras l1, (0,0), ('A', 'A'))
    | incrLetras l2 == ('A', 'A') = (l1, incrDigitos d, ('A', 'A'))
    | otherwise = (l1, d, incrLetras l2)

incrLetras :: Letras -> Letras
incrLetras ('Z', 'Z') = ('A', 'A')
incrLetras (l1, l2)
    | ord l2 == 90 = (chr (ord l1 +1), 'A')
    | otherwise = (l1, chr (ord l2 +1))

incrDigitos :: Digitos -> Digitos
incrDigitos (9,9) = (0,0)
incrDigitos (d1, d2)
    | d2 == 9 = (d1+1, 0)
    | otherwise = (d1, d2 +1)
