module MainTest exposing (scheduleTasTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)



-- TA examples


soba =
    TA "Soba" 2 [ 1, 3 ]


udon =
    TA "Udon" 1 [ 3, 4 ]


ramen =
    TA "Ramen" 1 [ 2 ]


noodleTAs =
    [ soba, udon, ramen ]


scheduleTasTest : Test
scheduleTasTest =
    describe "scheduleTas"
        [ test "return empty on empty tas and empty slots" <|
            \_ ->
                scheduleTas [] [] |> Expect.equal (Just [])
        , test "return Nothing on empty tas and non-empty slots" <|
            \_ ->
                scheduleTas [] [ 1, 2 ] |> Expect.equal Nothing
        , test "return empty on non-empty tas and empty slots" <|
            \_ ->
                scheduleTas [ soba ] [] |> Expect.equal (Just [])
        , test "return Just schedule if the schedule is possible" <|
            \_ ->
                scheduleTas [ soba ] [ 1 ] |> Expect.equal (Just [ Assignment soba 1 ])
        , test "return Nothing if the schedule is not possible" <|
            \_ ->
                scheduleTas [ soba ] [ 2 ] |> Expect.equal Nothing
        , test "return Just schedule on multiple slots" <|
            \_ ->
                scheduleTas [ soba ] [ 1, 3 ]
                    |> Expect.equal
                        (Just
                            [ Assignment soba 3
                            , Assignment soba 1
                            ]
                        )
        , test "return Just schedule on multiple Tas and slots" <|
            \_ ->
                scheduleTas noodleTAs [ 1, 2, 3, 4 ]
                    |> Expect.equal
                        (Just
                            [ Assignment udon 4
                            , Assignment soba 3
                            , Assignment ramen 2
                            , Assignment soba 1
                            ]
                        )
        , test "return Nothing on multiple Tas and slots when schedule is not possible" <|
            \_ ->
                scheduleTas noodleTAs [ 1, 2, 3, 4, 5 ] |> Expect.equal Nothing
        ]
