module MainTest exposing (addSlotFromIdTest, scheduleTasTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Random
import Test exposing (..)
import UUID



-- Slot examples


generateUUID : Int -> UUID.UUID
generateUUID i =
    let
        initialSeed =
            Random.initialSeed i

        ( uuid, _ ) =
            Random.step UUID.generator initialSeed
    in
    uuid


slot1 =
    Slot "13:00" "14:00" "Monday" (generateUUID 1)


slot2 =
    Slot "14:00" "15:00" "Monday" (generateUUID 2)


slot3 =
    Slot "15:00" "16:00" "Monday" (generateUUID 3)


slot4 =
    Slot "16:00" "17:00" "Monday" (generateUUID 4)


slot5 =
    Slot "16:00" "17:00" "Friday" (generateUUID 5)


slots =
    [ slot1, slot2, slot3, slot4 ]



-- TA examples


soba =
    TA "Soba" 2 [ slot1, slot3 ]


udon =
    TA "Udon" 1 [ slot3, slot4 ]


ramen =
    TA "Ramen" 1 [ slot2 ]


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
                scheduleTas [] [ slot1, slot2 ] |> Expect.equal Nothing
        , test "return empty on non-empty tas and empty slots" <|
            \_ ->
                scheduleTas [ soba ] [] |> Expect.equal (Just [])
        , test "return Just schedule if the schedule is possible" <|
            \_ ->
                scheduleTas [ soba ] [ slot1 ] |> Expect.equal (Just [ Assignment soba slot1 ])
        , test "return Nothing if the schedule is not possible" <|
            \_ ->
                scheduleTas [ soba ] [ slot2 ] |> Expect.equal Nothing
        , test "return Just schedule on multiple slots" <|
            \_ ->
                scheduleTas [ soba ] [ slot1, slot3 ]
                    |> Expect.equal
                        (Just
                            [ Assignment soba slot3
                            , Assignment soba slot1
                            ]
                        )
        , test "return Just schedule on multiple Tas and slots" <|
            \_ ->
                scheduleTas noodleTAs slots
                    |> Expect.equal
                        (Just
                            [ Assignment udon slot4
                            , Assignment soba slot3
                            , Assignment ramen slot2
                            , Assignment soba slot1
                            ]
                        )
        , test "return Nothing on multiple Tas and slots when schedule is not possible" <|
            \_ ->
                scheduleTas noodleTAs [ slot1, slot2, slot3, slot4, slot5 ] |> Expect.equal Nothing
        ]


addSlotFromIdTest : Test
addSlotFromIdTest =
    describe "addSlotFromId"
        [ test "return empty on empty slots and empty id" <|
            \_ ->
                addSlotFromId [] [] "" |> Expect.equal []
        , test "return unchanged list on empty slots" <|
            \_ ->
                let
                    uuid =
                        UUID.toString slot5.id
                in
                addSlotFromId [] [ slot1, slot2 ] uuid |> Expect.equal [ slot1, slot2 ]
        , test "return unchanged list of slot on empty id" <|
            \_ ->
                addSlotFromId slots [ slot1, slot2 ] "" |> Expect.equal [ slot1, slot2 ]
        , test "return unchanged list of slots on invalid id" <|
            \_ ->
                addSlotFromId slots [ slot1, slot2 ] "string" |> Expect.equal [ slot1, slot2 ]
        , test "add new slot to list of slot on valid id" <|
            \_ ->
                let
                    uuid =
                        UUID.toString slot5.id

                    allSlots =
                        slot5 :: slots
                in
                addSlotFromId allSlots [ slot1, slot2 ] uuid |> Expect.equal [ slot5, slot1, slot2 ]
        ]
