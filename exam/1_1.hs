type Letras = (Char, Char)
type Digitos = (Int, Int)
type Matricula = (Letras, Digitos, Letras)

valida :: Matricula -> Bool
valida ((l1,l2), (n1,n2), (l3,l4)) = validaLetras (l1, l2) && validaDigitos (n1, n2) && validaLetras (l3, l4)

validaLetras :: Letras -> Bool
validaLetras (l1, l2) = (l1 >= 'A') && (l2 >= 'A') && (l1 <= 'Z') && (l2 <= 'Z')

validaDigitos :: Digitos -> Bool
validaDigitos (d1, d2) = (d1 >= 0) && (d1 <= 9) && (d2 >= 0) && (d2 <= 9)