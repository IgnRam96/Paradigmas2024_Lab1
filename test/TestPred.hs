module Main (main) where
import Pred
import Test.HUnit

testPred :: Test
testPred = TestList [
    -- cambiar
    "Test 1" ~: cambiar (== 1) (const (figura 2)) (figura 1) ~?= figura 2,
    "Test 2" ~: cambiar (== 1) (const (figura 2)) (figura 3) ~?= figura 3,
    -- anyDib
    "Test 3" ~: anyDib (== 1) (figura 1) ~?= True,
    "Test 4" ~: anyDib (== 1) (figura 2) ~?= False,
    -- allDib
    "Test 5" ~: allDib (== 1) (figura 1) ~?= True,
    "Test 6" ~: allDib (== 1) (figura 2) ~?= False,
    -- andP
    "Test 7" ~: andP (== 1) (== 1) 1 ~?= True,
    "Test 8" ~: andP (== 1) (== 2) 1 ~?= False,
    "Test 9" ~: andP (== 1) (== 2) 2 ~?= False,
    "Test 10" ~: andP (== 1) (== 2) 3 ~?= False,
    -- orP
    "Test 11" ~: orP (== 1) (== 1) 1 ~?= True,
    "Test 12" ~: orP (== 1) (== 2) 1 ~?= True,
    "Test 13" ~: orP (== 1) (== 2) 2 ~?= True,
    "Test 14" ~: orP (== 1) (== 2) 3 ~?= False
    ]

main :: IO ()
main = do
    putStrLn "Running Test Suite for Pred."
    runTestTTAndExit testPred