module Main (main) where
import Dibujo
import Test.HUnit

testDibujo :: Test
testDibujo = TestList [
    "Test comp" ~: assertEqual "Should be equal in comp" (comp 1 (+1) 1) 2,
    "Test figura" ~: assertEqual "Should be equal in figura" (figura 1) (figura 1),
    "Test encimar" ~: assertEqual "Should be equal in encimar" (encimar (figura 1) (figura 2)) (encimar (figura 1) (figura 2)),
    "Test apilar" ~: assertEqual "Should be equal in apilar" (apilar 1 2 (figura 1) (figura 2)) (apilar 1 2 (figura 1) (figura 2)),
    "Test juntar" ~: assertEqual "Should be equal in juntar" (juntar 1 2 (figura 1) (figura 2)) (juntar 1 2 (figura 1) (figura 2)),
    "Test rot45" ~: assertEqual "Should be equal in rot45" (rot45 (figura 1)) (rot45 (figura 1)),
    "Test rot45(2)" ~: assertBool "Should be false in rot45" ((rot45 (juntar 1 1 (figura 1) (figura 2))) /= (juntar 1 1 (rot45(figura 1)) (rot45 (figura 2)))),
    "Test rotar" ~: assertEqual "Should be equal in rotar" (r90 (figura 1)) (rotar (figura 1)),
    "Test r180" ~: assertEqual "Should be equal in r180" (r180 (figura 1)) (rotar $ rotar (figura 1)),
    "Test r270" ~: assertEqual "Should be equal in r270" (r270 (figura 1)) (rotar $ rotar $ rotar (figura 1)),
    "Test espejar" ~: assertEqual "Should be equal in espejar" (espejar (figura 1)) (espejar (figura 1)),
    "Test encimar4" ~: assertEqual "Should be equal in encimar4" (encimar4 (figura 1)) ((^^^) (figura 1) ((^^^) (r90 (figura 1)) ((^^^) (r180 (figura 1)) (r270 (figura 1))))),
    "Test cuarteto" ~: assertEqual "Should be equal in cuarteto" (cuarteto (figura 1) (figura 2) (figura 3) (figura 4)) (apilar 1 1 (juntar 1 1 (figura 1) (figura 2)) (juntar 1 1 (figura 3) (figura 4))),
    "Test ciclar" ~: assertEqual "Should be equal in ciclar" (ciclar (figura 1)) (cuarteto (figura 1) (rotar (figura 1)) (r180 (figura 1)) (r270 (figura 1))),
    "Test mapDib" ~: assertEqual "Should be equal in mapDib" (mapDib (==1) (figura 1)) (figura (1 == 1)),
    "Test mapDib2" ~: assertEqual "Should be equal in mapDib2" (mapDib (==1) (espejar(figura 1))) (espejar (mapDib (==1) (figura 1))),
    "Test mapDib3" ~: assertEqual "Should be equal in mapDib3" (mapDib (>5) (juntar 1 1 (figura 1) (figura 2))) (juntar 1 1 (mapDib (>5) (figura 1)) (mapDib (>5) (figura 2))),
    "Test change" ~: assertEqual "Should be equal in change" (change (const (figura 2)) (rot45(figura 1))) (rot45 ((figura 2))),
    "Test change2" ~: assertEqual "Should be equal in change2" (change (const (figura 2)) (juntar 1 1 (figura 1) (figura 3))) (juntar 1 1 (figura 2) (figura 2)),
    "Test foldDib" ~: assertEqual "Should be equal in foldDib" (foldDib (figura) (rot45) (espejar) (rotar) (apilar) (juntar) (encimar) (rot45(figura 1))) (rot45(foldDib (figura) (rot45) (espejar) (rotar) (apilar) (juntar) (encimar) (figura 1))),
    "Test foldDib2" ~: assertEqual "Should be equal in foldDib2" (foldDib (figura) (rot45) (espejar) (rotar) (apilar) (juntar) (encimar) (encimar (figura 1) (figura 2))) (encimar (foldDib (figura) (rot45) (espejar) (rotar) (apilar) (juntar) (encimar) (figura 1))((foldDib (figura) (rot45) (espejar) (rotar) (apilar) (juntar) (encimar) (figura 2))))
    ]
main :: IO ()
main = do
    putStrLn "Running Test Suite for Dibujo."
    runTestTTAndExit testDibujo