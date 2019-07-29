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



-- interp. the TA's name, number of slots they can work, and slots they're available for


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
    Just []



-- stub
-- produce a Just schedule given TAs and Slots; Nothing if impossible
