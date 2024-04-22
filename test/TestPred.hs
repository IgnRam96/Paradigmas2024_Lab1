module Main (main) where
import Pred
import Dibujo (figura, espejar, apilar)
import Test.HUnit

testPred :: Test
testPred = TestList [
    -- cambiar
    "Test cambiar 1" ~: cambiar (== 1) (const (figura 2)) (figura 1) ~?= figura 2,
    "Test cambiar 2" ~: cambiar (== 1) (const (figura 2)) (figura 3) ~?= figura 3,
    "Test cambiar 3" ~: cambiar (== 1) (const (figura 2)) (apilar 1 1 (figura 1) (figura 3)) ~?= (apilar 1 1 (figura 2) (figura 3)),
    "Test cambiar 4" ~: cambiar (== 1) (const (figura 1)) (apilar 1 1 (figura 2) (figura 3)) ~?= apilar 1 1 (figura 2) (figura 3),
    -- anyDib
    "Test anyDib 1" ~: anyDib (== 1) (figura 1) ~?= True,
    "Test anyDib 2" ~: anyDib (== 1) (figura 2) ~?= False,
    "Test anyDib 3" ~: anyDib (== 1) (espejar(figura 1)) ~?= True,
    "Test anyDib 4" ~: anyDib (== 1) (espejar(figura 2)) ~?= False,
    "Test anyDib 5" ~: anyDib (== 1) (apilar 1 1 (figura 1) (figura 2)) ~?= True,
    "Test anyDib 6" ~: anyDib (== 1) (apilar 1 1 (figura 2) (figura 3)) ~?= False,
    -- allDib
    "Test allDib 1" ~: allDib (== 1) (figura 1) ~?= True,
    "Test allDib 2" ~: allDib (== 1) (figura 2) ~?= False,
    "Test allDib 3" ~: allDib (== 1) (espejar(figura 1)) ~?= True,
    "Test allDib 4" ~: allDib (== 1) (espejar(figura 2)) ~?= False,
    "Test allDib 5" ~: allDib (== 1) (apilar 1 1 (figura 1) (figura 1)) ~?= True,
    "Test allDib 6" ~: allDib (== 1) (apilar 1 1 (figura 1) (figura 2)) ~?= False,
    "Test allDib 7" ~: allDib (== 1) (apilar 1 1 (figura 2) (figura 3)) ~?= False,
    -- andP
    "Test andP 1" ~: andP (== 1) (== 1) 1 ~?= True,
    "Test andP 2" ~: andP (== 1) (== 2) 1 ~?= False,
    "Test andP 3" ~: andP (== 1) (== 2) 2 ~?= False,
    "Test andP 4" ~: andP (== 1) (== 2) 3 ~?= False,
    -- orP
    "Test orP 1" ~: orP (== 1) (== 1) 1 ~?= True,
    "Test orP 2" ~: orP (== 1) (== 2) 1 ~?= True,
    "Test orP 3" ~: orP (== 1) (== 2) 2 ~?= True,
    "Test orP 4" ~: orP (== 1) (== 2) 3 ~?= False
    ]

main :: IO ()
main = do
    putStrLn "Running Test Suite for Pred."
    runTestTTAndExit testPred