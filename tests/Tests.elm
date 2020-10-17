module Tests exposing (..)

import Dict exposing (remove)
import Expect
import Main exposing (countLiveNeighbours, getNeighbourCoords, isNeighbour, neighFunctions, removeDuplicates)
import Patterns exposing (glider)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")

        -- , test "This test should fail" <|
        --     \_ ->
        --         Expect.fail "failed as expected!"
        ]


testIsNeighbour : Test
testIsNeighbour =
    describe "Testing is Neighbour"
        [ test "Calculates neighbour correctly" <|
            \_ ->
                isNeighbour ( -1, -1 ) ( 0, 0 ) |> Expect.equal True
        , test "A cell is not a neighbour of itself" <|
            \_ ->
                isNeighbour ( 2, 3 ) ( 2, 3 ) |> Expect.equal False
        , test "Test not an actual neighbour" <|
            \_ ->
                isNeighbour ( 4, 5 ) ( 0, 0 ) |> Expect.equal False
        ]


testCountLiveNs : Test
testCountLiveNs =
    describe "Testing count Live Neighbours"
        [ test "Should show 3 neighbours for a cell in glider pattern" <|
            \_ ->
                countLiveNeighbours (glider 90 70) ( 34, 44 ) |> Expect.equal 3
        , test "Should show 1 neighbour for a cell in glider pattern" <|
            \_ ->
                countLiveNeighbours (glider 90 70) ( 34, 45 ) |> Expect.equal 1
        ]


testNeighFunctions : Test
testNeighFunctions =
    describe "Testing the list of functions that generate all neighbours"
        [ test "Test neighbours for (0, 0)" <|
            \_ -> List.map (\f -> f ( 0, 0 )) neighFunctions |> Expect.equal (getNeighbourCoords ( 0, 0 ))
        ]


testRemoveDuplicates : Test
testRemoveDuplicates =
    describe "Test remove duplicates"
        [ test "Removes duplicates" <|
            \_ -> removeDuplicates [ ( 1, 1 ), ( 2, 1 ), ( 2, 1 ), ( 2, 1 ) ] |> Expect.equal [ ( 1, 1 ), ( 2, 1 ) ]
        ]
