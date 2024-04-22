module Main (main) where
import Dibujo
import Test.HUnit

testDibujo :: Test
testDibujo = TestList [
    "Test comp" ~: assertEqual "Should be equal" (comp 1 (+1) 1) 2,
    "Test figura" ~: assertEqual "Should be equal" (figura 1) (figura 1),
    "Test encimar" ~: assertEqual "Should be equal" (encimar (figura 1) (figura 2)) (encimar (figura 1) (figura 2)),
    "Test apilar" ~: assertEqual "Should be equal" (apilar 1 2 (figura 1) (figura 2)) (apilar 1 2 (figura 1) (figura 2)),
    "Test juntar" ~: assertEqual "Should be equal" (juntar 1 2 (figura 1) (figura 2)) (juntar 1 2 (figura 1) (figura 2)),
    "Test rot45" ~: assertEqual "Should be equal" (rot45 (figura 1)) (rot45 (figura 1)),
    "Test rotar" ~: assertEqual "Should be equal" (rotar (figura 1)) (rotar (figura 1)),
    "Test r180" ~: assertEqual "Should be equal" (rotar(rotar (figura 1))) (r180 (figura 1)),
    "Test r270" ~: assertEqual "Should be equal" (rotar(rotar(rotar (figura 1)))) (r270 (figura 1)),
    "Test espejar" ~: assertEqual "Should be equal" (espejar (figura 1)) (espejar (figura 1)),
    "Test encimar4" ~: assertEqual "Should be equal" (encimar4 (figura 1)) (encimar4 (^^^) x ((^^^) (r90 x) ((^^^) (r180 x)(r270 x)))),
    "Test cuarteto" ~: assertEqual "Should be equal" (cuarteto (figura 1) (figura 2) (figura 3) (figura 4)) (apilar 1 1 (juntar 1 1 (figura 1) (figura 2)) (juntar 1 1 (figura 3) (figura 4))),
    "Test ciclar" ~: assertEqual "Should be equal" (ciclar (figura 1)) (cuarteto (figura 1) (rotar (figura 1)) (r180 (figura 1)) (r270 (figura 1))),
    "Test mapDib" ~: assertEqual "Should be equal" mapDib (mapDib (== 1) (figura 1)),
    "Test change" ~: assertEqual "Should be equal" change (== 1) (const (figura 2)) (figura 1),
    "Test foldDib" ~: assertEqual "Should be equal" foldDib (== 1) (&&) (figura 1)
    ]


main :: IO ()
main = do
    putStrLn "Running Test Suite for Dibujo."
    runTestTTAndExit testDibujo