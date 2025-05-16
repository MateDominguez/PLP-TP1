module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

-- Ejercicio 5
pponAtomico :: PPON -> Bool
pponAtomico (ObjetoPP _) = False
pponAtomico _ = True

-- Ejercicio 6
pponObjetoSimple :: PPON -> Bool
-- si es un ObjetoPP
pponObjetoSimple (ObjetoPP lista) = foldr (\x rec -> pponAtomico (snd x) && rec) True lista -- si todas las tuplas tiene valores atómicos retorna True de manera contraria retorna False
-- si no es un ObjetoPP
pponObjetoSimple _ = False -- no cumple con la condición por lo que retorna False

-- Ejercicio 7
intercalar :: Doc -> [Doc] -> Doc
intercalar sep [] = texto ""
intercalar sep = foldr1 (\x rec-> x <+> sep <+> rec)

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

-- >>> entreLlaves [texto "a", texto "b", texto "c", texto "d"]
-- Texto "{" (Linea 0 (Linea 2 (Texto "a" (Texto "," (Linea 2 (Texto "b" (Texto "," (Linea 2 (Texto "c" (Texto "," (Linea 2 (Texto "d" (Linea 0 (Texto "}" Vacio))))))))))))))

-- Ejercicio 8
aplanar :: Doc -> Doc
aplanar =
  foldDoc
    vacio -- Caso Vacio
    (\s rec -> texto s <+> rec) -- Caso Texto: sigue la recursion
    (\i rec -> texto " " <+> rec) -- Caso Linea: sigue la recursion

-- Ejercicio 9
pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s)
pponADoc (IntPP i) = texto (show i)
pponADoc (ObjetoPP l) = 
  if pponObjetoSimple (ObjetoPP l)
    then texto "{ " <+> intercalar (texto ", ") (map formatear l) <+> texto " }" -- pponSimple
    else entreLlaves (map formatear l) -- pponComplejo
  where
    formatear = (\(key,rec) -> texto (show key ++ ": ") <+> pponADoc rec)

-- La función utiliza recursion primitiva.
-- En los casos base (TextoPP y IntPP) devolvemos un valor fijo
-- En el caso recursivo (ObjetoPP l):
--   * Usamos l para decidir si PPON es pponObjetoSimple
--   * Hacemos el llamado recursivo sobre los elementos de l (snd x)
-- Como usamos l tanto para el llamado recursivo como para otras operaciones, 
-- podemos decir que es recursion primitiva.