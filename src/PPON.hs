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
intercalar sep = foldr1 (\x rec -> x <+> sep <+> rec)

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
    (\s rec -> if rec == vacio then texto s <+> rec else texto (s ++ " ") <+> rec)
    -- Caso Texto: concatena un string con un espacio al final si el documento siguiente no es vacio
    (\i rec -> rec) -- Caso Linea: sigue la recursion

-- Ejercicio 9
pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s)
pponADoc (IntPP i) = texto (show i)
pponADoc (ObjetoPP l) = if pponObjetoSimple (ObjetoPP l) then pponSimple else pponComplejo
  where
    pponComplejo = entreLlaves (map (\x -> texto (show (fst x) ++ " : ") <+> pponADoc (snd x)) l)
    pponSimple = texto "{" <+> intercalar (texto ",") (map (\x -> texto (show (fst x) ++ ":") <+> pponADoc (snd x)) l) <+> texto "}"
