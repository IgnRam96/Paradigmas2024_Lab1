module Interp
  ( interp,
    initial,
    ov,
    r45,
    rot,
    esp,
    sup,
    jun,
    api
  )
where

import Dibujo
import FloatingPic
import Graphics.Gloss (Display (InWindow), color, display, makeColorI, pictures, translate, white, Picture)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial (Conf n dib intBas) size = display win white $ withGrid fig size
  where
    win = InWindow n (ceiling size, ceiling size) (0, 0)
    fig = interp intBas dib (0, 0) (size, 0) (0, size)
    desp = -(size / 2)
    withGrid p x = translate desp desp $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
    grey = makeColorI 100 100 100 100

-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
ov p q = pictures [p, q]

r45 :: FloatingPic -> FloatingPic
r45 f d w h = f (d V.+ half(w V.+ h)) (half(w V.+ h)) (half(h V.- w))

rot :: FloatingPic -> FloatingPic
rot f d w h = f (d V.+ w) h (zero V.- w)

esp :: FloatingPic -> FloatingPic
esp f d w h = f (d V.+ w) (zero V.- w) h

sup :: FloatingPic -> FloatingPic -> FloatingPic
sup f g d w h = ov (f d w h) (g d w h)

jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun x y f g d w h = ov (f d w' h) (g (d V.+ w') (r' V.* w) (h))
  where
    -- r y r' son factores de escalamiento segun los numeros flotantes x e y, y w' es el vector h escalado
    r = x / (x + y)
    r' = y / (x + y)
    w' = w V.* r

api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api x y f g d w h = ov (f (d V.+ h') w (r V.* h)) (g d w h')
  where
    -- r y r' son factores de escalamiento segun los numeros flotantes x e y, y h' es el vector h escalado
    r = x / (x + y)
    r' = y / (x + y)
    h' = h V.* r'

interp :: Output a -> Output (Dibujo a)
interp f = foldDib f r45 rot esp sup jun api