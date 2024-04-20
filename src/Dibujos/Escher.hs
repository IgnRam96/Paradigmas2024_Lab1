module Dibujos.Escher where

import Dibujo
import FloatingPic(Conf(..), Output, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss (Picture(Blank) ,black, color, line, pictures)
import Dibujos.Grilla (grilla)


type Escher = Bool

-- dibujo basico de escher con 2 figuras
dibujoBas :: Dibujo Escher -> Dibujo Escher
dibujoBas p = espejar (rot45 p)

-- dibujo basico de escher con 3 figuras
dibujoBas2 :: Dibujo Escher -> Dibujo Escher
dibujoBas2 p = r270 (dibujoBas p)

-- interpretacion de una figura basica
interpBas:: Output Escher
interpBas False _ _ _ = Blank
interpBas True x y w = color black $ pictures [line $ map (x V.+) [zero, w, y, zero]]

-- El dibujo u.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar4 (espejar (rot45 p))

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = encimar p (encimar (dibujoBas p) (dibujoBas2 p))

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 p = cuarteto (figura False) (figura False) (figura False) (dibujoU p)
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) p) (rotar (lado (n-1) p)) (dibujoU p)

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 p = cuarteto (figura False) (figura False) (rotar (dibujoT p)) (dibujoT p)
lado n p = cuarteto (lado (n-1) p) (lado (n-1) p) (rotar (dibujoT p)) (dibujoT p)

-- Por suerte no tenemos que poner el tipo!
noneto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
noneto p q r s t u v w x = grilla [
    [p, q, r],
    [s, t, u],
    [v, w, x]
    ]

-- El dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher n x = noneto 
        (esquina n (figura x)) (lado n (figura x)) (r270 (esquina n (figura x))) 
        (rotar (lado n (figura x))) (dibujoU (figura x)) (r270 (lado n (figura x))) 
        (rotar (esquina n (figura x))) (r180 (lado n (figura x))) (r180 (esquina n (figura x)))

-- configuracion de escher
escherConf :: Conf
escherConf = Conf {
    name = "Escher"
    , pic = escher 4 True
    , bas = interpBas
}