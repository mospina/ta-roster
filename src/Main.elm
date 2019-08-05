module Main exposing (Assignment, Schedule, Slot, TA, scheduleTas)

{-
   PROBLEM 2:

   In UBC's version of How to Code, there are often more than 800 students taking
   the course in any given semester, meaning there are often over 40 Teaching Assistants.

   Designing a schedule for them by hand is hard work - luckily we've learned enough now to write
   a program to do it for us!

   Below are some data definitions for a simplified version of a TA schedule. There are some
   number of slots that must be filled, each represented by a natural number. Each TA is
   available for some of these slots, and has a maximum number of shifts they can work.

   Design a search program that consumes a list of TAs and a list of Slots, and produces one
   valid schedule where each Slot is assigned to a TA, and no TA is working more than their
   maximum shifts. If no such schedules exist, produce false.

   You should supplement the given check-expects and remember to follow the recipe!
-}


type alias Slot =
    Int



-- interp. each TA slot has a number, is the same length, and none overlap
--         it can be thougth as an ID.


type alias TA =
    { name : String
    , max : Int
    , avail : List Slot
    }



{-
   -- interp. the TA's name, number of slots they can work, and slots they're available for
   fnForTa : TA -> ...
   fnForTa ta = ... ta.name ta.max (fnForLos ta.avail)

   fnForLot : List TA -> ...
   fnForLot ta =
     case ta of
       [] -> []
       first :: rest -> ... (fnForTa first)
                            (fnForLot rest)
-}


type alias Assignment =
    { ta : TA
    , slot : Slot
    }



-- interp. the TA is assigned to work the slot


type alias Schedule =
    List Assignment



-- FUNCTIONS


scheduleTas : List TA -> List Slot -> Maybe Schedule
scheduleTas tas slots =
    let
        fnForS tas0 slots0 schedule =
            case slots0 of
                [] ->
                    Just schedule

                first :: rest ->
                    fnForLos tas0 rest schedule (nextAssignments first tas0)

        fnForLos tas1 slots1 schedule1 assignments =
            case assignments of
                [] ->
                    Nothing

                first :: rest ->
                    let
                        try =
                            fnForS (reduceTasMax tas1 first) slots1 (first :: schedule1)
                    in
                    case try of
                        Just schedule ->
                            Just (resetTas schedule)

                        _ ->
                            fnForLos tas1 slots1 schedule1 rest

        nextAssignments : Slot -> List TA -> List Assignment
        nextAssignments slot2 tas2 =
            let
                fnForTa ta =
                    List.member slot2 ta.avail

                fnForLot lot acc =
                    case lot of
                        [] ->
                            acc

                        first :: rest ->
                            if fnForTa first then
                                fnForLot rest (Assignment first slot2 :: acc)

                            else
                                fnForLot rest acc
            in
            fnForLot tas2 []

        reduceTasMax : List TA -> Assignment -> List TA
        reduceTasMax tas3 assignment =
            let
                fnForTa ta =
                    if assignment.ta == ta then
                        { ta | max = ta.max - 1 }

                    else
                        ta
            in
            List.map fnForTa tas3 |> List.filter (\ta -> ta.max > 0)

        resetTas : Schedule -> Schedule
        resetTas schedule =
            let
                replaceTa assignment =
                    { assignment | ta = getTa assignment.ta tas }

                getTa ta tas4 =
                    case tas4 of
                        [] ->
                            ta

                        first :: rest ->
                            if first.name == ta.name then
                                first

                            else
                                getTa ta rest
            in
            List.map replaceTa schedule
    in
    fnForS tas slots []



{-
   scheduleTas : List TA -> List Slot -> Maybe Schedule
   scheduleTas tas slots =
       case slots of
           [] ->
               Just []                      -- find solution, return schedule

           firstSlot :: restSlot ->         -- go into each slot and generate possible schedule
               case tas of
                   [] ->
                       Nothing              -- invalid schedule

                   firstTas :: restTas ->
                       Nothing

-}
-- stub
-- produce a Just schedule given TAs and Slots; Nothing if impossible
-- Two one of data.
-- backtracking search (to check valid options (like on slot 3 4)
-- Generative to create the Schedule
{-
         CROSS PRODUCT OF TYPE COMMENTS TABLE


         +----------------------------------------------+
         | tas \ slots | []      | List Slot            |
         |----------------------------------------------|
         | []          |         | Nothing              |
         |-------------| Just [] |----------------------|
         | List TA     |         | Schedule             |
         +----------------------------------------------+

   scheduleTas
    [ TA "Soba" 2 [ 1, 3 ]
    , TA "Udon" 1 [ 3, 4 ]
    , TA "Ramen" 1 [ 2 ]
    ]
    [ 1, 2, 3, 4 ]


   Just ([Assignment "Soba" 1])
   ScheduleTas
    [ TA "Soba" 1 [ 1, 3 ]
    , TA "Udon" 1 [ 3, 4 ]
    , TA "Ramen" 1 [ 2 ]
    ]
    [ 2, 3, 4 ]

   Just ([ Assignment "Soba" 1
        , Assignment "Ramen" 2
        ])
   ScheduleTas
    [ TA "Soba" 1 [ 1, 3 ]
    , TA "Udon" 1 [ 3, 4 ]
    , TA "Ramen" 0 [ 2 ]
    ]
    [ 3, 4 ]

   -- Branch 1: --
   Just ([ Assignment "Soba" 1
        , Assignment "Ramen" 2
        , Assignment "Soba" 3
        ])
   ScheduleTas
    [ TA "Soba" 0 [ 1, 3 ]
    , TA "Udon" 1 [ 3, 4 ]
    , TA "Ramen" 0 [ 2 ]
    ]
    [ 4 ]

   Just ([ Assignment "Soba" 1
        , Assignment "Ramen" 2
        , Assignment "Soba" 3
        , Assignment "Udon" 4
        ])
   ScheduleTas
    [ TA "Soba" 0 [ 1, 3 ]
    , TA "Udon" 0 [ 3, 4 ]
    , TA "Ramen" 0 [ 2 ]
    ]
    [ ]

   -- Branch 2: --

   Just ([ Assignment "Soba" 1
        , Assignment "Ramen" 2
        , Assignment "Udon" 3
        ])
   ScheduleTas
    [ TA "Soba" 1 [ 1, 3 ]
    , TA "Udon" 0 [ 3, 4 ]
    , TA "Ramen" 0 [ 2 ]
    ]
    [ 4 ]

   Just ([ Assignment "Soba" 1
        , Assignment "Ramen" 2
        , Assignment "Udon" 3
        , Nothing
        ])
   ScheduleTas
    [ TA "Soba" 1 [ 1, 3 ]
    , TA "Udon" 0 [ 3, 4 ]
    , TA "Ramen" 0 [ 2 ]
    ]
    []
-}