module Pred(
  Pred,
  cambiar, anyDib, allDib, orP, andP, falla
) where

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pred f = mapDib (\x -> if pred x then f x else x)

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib predic = foldDib predic id id id (\_ _ x y -> (x || y)) (\_ _ x y -> (x || y)) (||)
-- Notar que (||) = \x y = (x||y) {Es unicamente notacion distinta}
-- \ Nos define una funcion delta
-- Usamos foldDib para no usar pattern-matching 

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib predic = foldDib predic id id id (\_ _ x y -> (x && y)) (\_ _ x y -> (x && y)) (&&)

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP predic_a predic_b x = (predic_a x) && (predic_b x)

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP predic_a predic_b x = (predic_a x) || (predic_b x)

-- falla = True