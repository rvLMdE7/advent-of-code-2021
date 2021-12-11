{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test11 where

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Common.Matrix (Matrix, liftMatrix)
import Day11 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ smallExample
    , bigExample
    ]

smallExample :: TestTree
smallExample = Tasty.testGroup "5x5 example"
    [ HUnit.testCase "after 0 steps" $
        fst (Day11.flashes 0 example5x5 9) @?= example5x5
    , HUnit.testCase "after 1 steps" $
        let after1 :: Matrix 5 5 Int =
                $$(
                    liftMatrix
                        [ [3, 4, 5, 4, 3]
                        , [4, 0, 0, 0, 4]
                        , [5, 0, 0, 0, 5]
                        , [4, 0, 0, 0, 4]
                        , [3, 4, 5, 4, 3]
                        ]
                )
        in  fst (Day11.flashes 1 example5x5 9) @?= after1
    , HUnit.testCase "after 2 steps" $
        let after2 :: Matrix 5 5 Int =
                $$(
                    liftMatrix
                        [ [4, 5, 6, 5, 4]
                        , [5, 1, 1, 1, 5]
                        , [6, 1, 1, 1, 6]
                        , [5, 1, 1, 1, 5]
                        , [4, 5, 6, 5, 4]
                        ]
                )
        in  fst (Day11.flashes 2 example5x5 9) @?= after2
    ]

bigExample :: TestTree
bigExample = Tasty.testGroup "10x10 example"
    [ HUnit.testCase "after   0 steps" $
        Day11.flashes 0 example10x10 9 @?= (example10x10, 0)
    , HUnit.testCase "after   1 steps" $
        let after1 :: Matrix 10 10 Int =
                $$(
                    liftMatrix
                        [ [6, 5, 9, 4, 2, 5, 4, 3, 3, 4]
                        , [3, 8, 5, 6, 9, 6, 5, 8, 2, 2]
                        , [6, 3, 7, 5, 6, 6, 7, 2, 8, 4]
                        , [7, 2, 5, 2, 4, 4, 7, 2, 5, 7]
                        , [7, 4, 6, 8, 4, 9, 6, 5, 8, 9]
                        , [5, 2, 7, 8, 6, 3, 5, 7, 5, 6]
                        , [3, 2, 8, 7, 9, 5, 2, 8, 3, 2]
                        , [7, 9, 9, 3, 9, 9, 2, 2, 4, 5]
                        , [5, 9, 5, 7, 9, 5, 9, 6, 6, 5]
                        , [6, 3, 9, 4, 8, 6, 2, 6, 3, 7]
                        ]
                )
        in  fst (Day11.flashes 1 example10x10 9) @?= after1
    , HUnit.testCase "after   3 steps" $
        let after3 :: Matrix 10 10 Int =
                $$(
                    liftMatrix
                        [ [0, 0, 5, 0, 9, 0, 0, 8, 6, 6]
                        , [8, 5, 0, 0, 8, 0, 0, 5, 7, 5]
                        , [9, 9, 0, 0, 0, 0, 0, 0, 3, 9]
                        , [9, 7, 0, 0, 0, 0, 0, 0, 4, 1]
                        , [9, 9, 3, 5, 0, 8, 0, 0, 6, 3]
                        , [7, 7, 1, 2, 3, 0, 0, 0, 0, 0]
                        , [7, 9, 1, 1, 2, 5, 0, 0, 0, 9]
                        , [2, 2, 1, 1, 1, 3, 0, 0, 0, 0]
                        , [0, 4, 2, 1, 1, 2, 5, 0, 0, 0]
                        , [0, 0, 2, 1, 1, 1, 9, 0, 0, 0]
                        ]
                )
        in  fst (Day11.flashes 3 example10x10 9) @?= after3
    , HUnit.testCase "after   6 steps" $
        let after6 :: Matrix 10 10 Int =
                $$(
                    liftMatrix
                        [ [5, 5, 9, 5, 2, 5, 5, 1, 1, 1]
                        , [3, 1, 5, 5, 2, 5, 5, 2, 2, 2]
                        , [3, 3, 6, 4, 4, 4, 4, 6, 0, 5]
                        , [2, 2, 6, 3, 4, 4, 4, 4, 9, 6]
                        , [2, 2, 9, 8, 4, 1, 4, 3, 9, 6]
                        , [2, 2, 7, 5, 7, 4, 4, 3, 4, 4]
                        , [2, 2, 6, 4, 5, 8, 3, 3, 4, 2]
                        , [7, 7, 5, 4, 4, 6, 3, 3, 4, 4]
                        , [3, 7, 5, 4, 4, 6, 9, 4, 3, 3]
                        , [3, 3, 5, 4, 4, 5, 2, 4, 3, 3]
                        ]
                )
        in  fst (Day11.flashes 6 example10x10 9) @?= after6
    , HUnit.testCase "after  10 steps" $
        let after10 :: Matrix 10 10 Int =
                $$(
                    liftMatrix
                        [ [0, 4, 8, 1, 1, 1, 2, 9, 7, 6]
                        , [0, 0, 3, 1, 1, 1, 2, 0, 0, 9]
                        , [0, 0, 4, 1, 1, 1, 2, 5, 0, 4]
                        , [0, 0, 8, 1, 1, 1, 1, 4, 0, 6]
                        , [0, 0, 9, 9, 1, 1, 1, 3, 0, 6]
                        , [0, 0, 9, 3, 5, 1, 1, 2, 3, 3]
                        , [0, 4, 4, 2, 3, 6, 1, 1, 3, 0]
                        , [5, 5, 3, 2, 2, 5, 2, 3, 5, 0]
                        , [0, 5, 3, 2, 2, 5, 0, 6, 0, 0]
                        , [0, 0, 3, 2, 2, 4, 0, 0, 0, 0]
                        ]
                )
        in  Day11.flashes 10 example10x10 9 @?= (after10, 204)
    , HUnit.testCase "after  30 steps" $
        let after30 :: Matrix 10 10 Int =
                $$(
                    liftMatrix
                        [ [0, 6, 4, 3, 3, 3, 4, 1, 1, 8]
                        , [4, 2, 5, 3, 3, 3, 4, 6, 1, 1]
                        , [3, 3, 7, 4, 3, 3, 3, 4, 5, 8]
                        , [2, 2, 2, 5, 3, 3, 3, 3, 3, 7]
                        , [2, 2, 2, 9, 3, 3, 3, 3, 3, 8]
                        , [2, 2, 7, 6, 7, 3, 3, 3, 3, 3]
                        , [2, 7, 5, 4, 5, 7, 4, 5, 6, 5]
                        , [5, 5, 4, 4, 4, 5, 8, 5, 1, 1]
                        , [9, 4, 4, 4, 4, 4, 7, 1, 1, 1]
                        , [7, 9, 4, 4, 4, 4, 6, 1, 1, 9]
                        ]
                )
        in  fst (Day11.flashes 30 example10x10 9) @?= after30
    , HUnit.testCase "after  70 steps" $
        let after70 :: Matrix 10 10 Int =
                $$(
                    liftMatrix
                        [ [8, 2, 1, 1, 1, 1, 1, 1, 6, 4]
                        , [0, 4, 2, 1, 1, 1, 1, 1, 6, 6]
                        , [0, 0, 4, 2, 1, 1, 1, 1, 1, 4]
                        , [0, 0, 0, 4, 2, 1, 1, 1, 1, 5]
                        , [0, 0, 0, 0, 2, 1, 1, 1, 1, 6]
                        , [0, 0, 6, 5, 6, 1, 1, 1, 1, 1]
                        , [0, 5, 3, 2, 3, 5, 1, 1, 1, 1]
                        , [7, 3, 2, 2, 2, 3, 5, 1, 1, 7]
                        , [5, 7, 2, 2, 2, 2, 3, 4, 7, 5]
                        , [4, 5, 7, 2, 2, 2, 2, 7, 5, 4]
                        ]
                )
        in  fst (Day11.flashes 70 example10x10 9) @?= after70
    , HUnit.testCase "after 100 steps" $
        let after100 :: Matrix 10 10 Int =
                $$(
                    liftMatrix
                        [ [0, 3, 9, 7, 6, 6, 6, 8, 6, 6]
                        , [0, 7, 4, 9, 7, 6, 6, 9, 1, 8]
                        , [0, 0, 5, 3, 9, 7, 6, 9, 3, 3]
                        , [0, 0, 0, 4, 2, 9, 7, 8, 2, 2]
                        , [0, 0, 0, 4, 2, 2, 9, 8, 9, 2]
                        , [0, 0, 5, 3, 2, 2, 2, 8, 7, 7]
                        , [0, 5, 3, 2, 2, 2, 2, 9, 6, 6]
                        , [9, 3, 2, 2, 2, 2, 8, 9, 6, 6]
                        , [7, 9, 2, 2, 2, 8, 6, 8, 6, 6]
                        , [6, 7, 8, 9, 9, 9, 8, 7, 6, 6]
                        ]
                )
        in  Day11.flashes 100 example10x10 9 @?= (after100, 1656)
    ]

example5x5 :: Matrix 5 5 Int
example5x5 =
    $$(
        liftMatrix
            [ [1, 1, 1, 1, 1]
            , [1, 9, 9, 9, 1]
            , [1, 9, 1, 9, 1]
            , [1, 9, 9, 9, 1]
            , [1, 1, 1, 1, 1]
            ]
    )

example10x10 :: Matrix 10 10 Int
example10x10 =
    $$(
        liftMatrix
            [ [5, 4, 8, 3, 1, 4, 3, 2, 2, 3]
            , [2, 7, 4, 5, 8, 5, 4, 7, 1, 1]
            , [5, 2, 6, 4, 5, 5, 6, 1, 7, 3]
            , [6, 1, 4, 1, 3, 3, 6, 1, 4, 6]
            , [6, 3, 5, 7, 3, 8, 5, 4, 7, 8]
            , [4, 1, 6, 7, 5, 2, 4, 6, 4, 5]
            , [2, 1, 7, 6, 8, 4, 1, 7, 2, 1]
            , [6, 8, 8, 2, 8, 8, 1, 1, 3, 4]
            , [4, 8, 4, 6, 8, 4, 8, 5, 5, 4]
            , [5, 2, 8, 3, 7, 5, 1, 5, 2, 6]
            ]
    )
