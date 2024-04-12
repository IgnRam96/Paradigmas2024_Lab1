module Dibujo (Dibujo,
              figura,
              encimar, 
              apilar, 
              juntar, 
              rot45, 
              rotar, 
              espejar, 
              (^^^), 
              (.-.), 
              (///), 
              r90, 
              r180, 
              r270, 
              encimar4, 
              cuarteto, 
              ciclar, 
              mapDib, 
              change, 
              foldDib
    ) where


-- nuestro lenguaje 
data Dibujo a = Figura a
              | Encimar (Dibujo a) (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Rot45 (Dibujo a)
              | Rotar (Dibujo a)
              | Espejar (Dibujo a)
              deriving(Eq, Show)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp 0 f a = a
comp n f a = f (a comp (n-1) f a)
-- comp -n f a = error

-- Funciones constructoras
figura :: a -> Dibujo a
figura = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar =  Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

-- Combinadores 

(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) x y = encimar 1 1 x y

-- Pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) x y = apilar 1 1 x y

-- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) x y = juntar 1 1 x y

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 x = rotar a

r180 :: Dibujo a -> Dibujo a
r180 x = comp 2 r90 x

r270 :: Dibujo a -> Dibujo a
r270 x = comp 3 r90 x

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 x = (^^^) x ((^^^) (r90 x) ((^^^) (r180 x)(r270 x)))

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto x y z w = (///) ((.-.) x y) ((.-.) z w)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar = cuarteto x (rotar x) (r180 x) (r270 x)

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib figura x = x
mapDib f x = case f of
    Encimar a b -> Encimar (mapDib f a) (mapDib f b)
    Apilar a b c d -> Apilar a b (mapDib f c) (mapDib f d)
    Juntar a b c d -> Juntar a b (mapDib f c) (mapDib f d)
    Rot45 a -> Rot45 (mapDib f a)
    Rotar a -> Rotar (mapDib f a)
    Espejar a -> Espejar (mapDib f a)
-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change = undefined

-- Principio de recursión para Dibujos.
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib = undefined