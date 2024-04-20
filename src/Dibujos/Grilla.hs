module Dibujos.Grilla where

import Dibujo (Dibujo, figura, juntar, apilar)
import FloatingPic(Conf(..), Output)
import Graphics.Gloss (black, color, translate)
import Graphics.Gloss.Data.Picture (text, scale)

type Basica = (Int, Int)

interpBasicaSinColor :: Output Basica
interpBasicaSinColor (n,m) (x,y) _ _= translate x y (scale 0.2 0.2(text c))
    where
        c = "(" ++ show n ++ "," ++ show m ++ ")"

interpBas :: Output Basica
interpBas b x y w = color black $ interpBasicaSinColor b x y w

basCoords :: Int -> Int -> Dibujo Basica
basCoords n m = figura (n, m)

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

makeRow:: Int -> Int -> [Dibujo Basica]
makeRow _ 0 = []
makeRow n m = makeRow n (m-1) ++ [basCoords n (m-1)]

makeColum:: Int -> Int -> [[Dibujo Basica]]
makeColum 0 _ = []
makeColum n m = makeColum (n-1) m ++ [makeRow (n-1) m]

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

--Cambiar numeros en makeColum para cambiar numeracion en la grilla
testAll :: Dibujo Basica
testAll = grilla (makeColum 8 8)

grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla"
    , pic = testAll
    , bas = interpBas
}