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
comp 1 f = f
comp n f = f . comp (n-1) f
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

-- Pone el primer dibujo arriba del segundo, el segundo dibujo se ve por encima del primero
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = encimar

-- Pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = apilar 1 1

-- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = juntar 1 1

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 = rotar 

r180 :: Dibujo a -> Dibujo a
r180 = comp 2 r90 

r270 :: Dibujo a -> Dibujo a
r270 = comp 3 r90 

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 x = (^^^) x ((^^^) (r90 x) ((^^^) (r180 x)(r270 x)))

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto x y z w = (.-.) ((///) x y) ((///) z w)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar x = cuarteto x (rotar x) (r180 x) (r270 x)

-- map para nuestro lenguaje
-- matcheamos la funcion f con los constructores de dibujo y aplicamos la funcion.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Figura a) = Figura (f a)
mapDib f (Encimar a b) = Encimar (mapDib f a) (mapDib f b)
mapDib f (Apilar x y a b) = Apilar x y (mapDib f a) (mapDib f b)
mapDib f (Juntar x y a b) = Juntar x y (mapDib f a) (mapDib f b)
mapDib f (Rot45 a) = Rot45 (mapDib f a)
mapDib f (Rotar a) = Rotar (mapDib f a)
mapDib f (Espejar a) = Espejar (mapDib f a)

-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change f (Figura a) = f a
change f (Encimar a b) = Encimar (change f a) (change f b)
change f (Apilar x y a b) = Apilar x y (change f a) (change f b)
change f (Juntar x y a b) = Juntar x y (change f a) (change f b)
change f (Rot45 a) = Rot45 (change f a)
change f (Rotar a) = Rotar (change f a)
change f (Espejar a) = Espejar (change f a)

-- Principio de recursión para Dibujos.
-- matcheamos la funcion f con los constructores de dibujo y aplicamos la funcion.
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
foldDib f _ _ _ _ _ _ (Figura a) = f a
foldDib f g h i j k l (Rot45 a) = g (foldDib f g h i j k l a)
foldDib f g h i j k l (Espejar a) = h (foldDib f g h i j k l a)
foldDib f g h i j k l (Rotar a) = i (foldDib f g h i j k l a)
foldDib f g h i j k l (Apilar x y a b) = j x y (foldDib f g h i j k l a) (foldDib f g h i j k l b)
foldDib f g h i j k l (Juntar x y a b) = k x y (foldDib f g h i j k l a) (foldDib f g h i j k l b)
foldDib f g h i j k l (Encimar a b) = l (foldDib f g h i j k l a) (foldDib f g h i j k l b)