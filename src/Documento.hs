module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

-- Ejercicio 1
foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc cVacio cTexto cLinea Vacio = cVacio -- Caso base Vacio
foldDoc cVacio cTexto cLinea (Texto s d) = cTexto s (foldDoc cVacio cTexto cLinea d) -- Caso recursivo Texto
foldDoc cVacio cTexto cLinea (Linea i d) = cLinea i (foldDoc cVacio cTexto cLinea d) -- Caso recursivo Linea

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

--  Ejercicio 2
(<+>) :: Doc -> Doc -> Doc
(<+>) d1 d2 =
  foldDoc
    d2 -- Caso Vacio: Devuelvo d2.
    ( \s1 rec -> -- Caso Texto: Si el llamado recursivo es de tipo Texto ... 
            case rec of
              Texto s2 d2' -> Texto (s1 ++ s2) d2' -- concateno el string del llamado recursivo (s2), al string de la función lambda (s1) ...
              _ -> Texto s1 rec -- caso Contrario, sigue la recursion.
    )
    (\i rec -> Linea i rec) -- Caso Linea: Agrega la linea y sigue la recursion.
    d1

-- Ejercicio 3
indentar :: Int -> Doc -> Doc
indentar i d =
  foldDoc
    Vacio -- Caso Vacio
    Texto -- Caso Texto
    (\i' rec -> Linea (i+i') rec)
    d

-- Ejercicio 4
mostrar :: Doc -> String
mostrar d =
  foldDoc
    "" -- Caso Vacio
    (++) -- Caso Texto
    (\i rec -> "\n" ++ (nEspacios [1 .. i]) ++ rec) -- Caso Linea
    d

nEspacios :: [Int] -> String
nEspacios = foldr (\x rec -> " " ++ rec) ""

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def j

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
