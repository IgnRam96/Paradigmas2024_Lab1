module Dibujos.Grilla where

import Dibujo (Dibujo, figura, juntar, apilar, rotar, encimar, espejar) --rot45
import FloatingPic(Conf(..), Output) -- half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, blue, red, black, yellow,color, line, translate) -- pictures )
import Graphics.Gloss.Data.Picture (text)
import Lucid.Svg.Attributes (y1_)

-- Les ponemos colorcitos para que no sea _tan_ feo
data Color = Azul | Rojo | Amarillo | Negro
    deriving (Show, Eq)

data BasicaSinColor = Texto_1 | Texto_2 | Texto_3 | Texto_4
    deriving (Show, Eq)

type Basica = (BasicaSinColor, Color)

colorear :: Color -> Picture -> Picture
colorear Azul = color blue
colorear Rojo = color red
colorear Amarillo = color yellow
colorear Negro = color black

-- Las coordenadas que usamos son:
--
--  x + y
--  |
--  x --- x + w
--
-- por ahi deban ajustarlas
interpBasicaSinColor2 :: Output BasicaSinColor
interpBasicaSinColor2 Texto_1 (x,y) _ _= translate x y (text "1")
interpBasicaSinColor2 Texto_2 (x,y) _ _= translate x y (text "2")
interpBasicaSinColor2 Texto_3 (x,y) _ _= translate x y (text "3")
interpBasicaSinColor2 Texto_4 (x,y) _ _= translate x y (text "4")

interpBas :: Output Basica
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor2 b x y w


-- Diferentes tests para ver que estén bien las operaciones
figRoja :: BasicaSinColor -> Dibujo Basica
figRoja b = figura (b, Rojo)

figAzul :: BasicaSinColor -> Dibujo Basica
figAzul b = figura (b, Azul)

figAmar :: BasicaSinColor -> Dibujo Basica
figAmar b = figura (b, Amarillo)

figNegr :: BasicaSinColor -> Dibujo Basica
figNegr b = figura (b, Negro)


row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar (fromIntegral $ length ds) 1 d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar (fromIntegral $ length ds) 1 d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

testAll :: Dibujo Basica
testAll = grilla [
    [figRoja Texto_1  , figRoja Texto_2  , figRoja Texto_3  , figRoja Texto_4],
    [figAzul Texto_1  , figAzul Texto_2  , figAzul Texto_3  , figAzul Texto_4],
    [figAmar Texto_1  , figAmar Texto_2  , figAmar Texto_3  , figAmar Texto_4],
    [figNegr Texto_1  , figNegr Texto_2  , figNegr Texto_3  , figNegr Texto_4]
    ]

grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla"
    , pic = testAll
    , bas = interpBas
}