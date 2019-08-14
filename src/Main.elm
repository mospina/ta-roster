module Main exposing (Assignment, RosterTime, Schedule, Slot, TA, scheduleTas)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import UUID exposing (UUID)
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias RosterTime =
    { hour : Int
    , min : Int
    }



-- interp. each roster time has an hour and min(utes).
--         Hour is [1,24], minute is [0, 59]
--
-- an hour = RosterTime 3 15
{-
   fnForRosterTime : RosterTime -> ...
   fnForRosterTime time =
     ... time.hour  -- Int
         time.min   -- Int

   -- Template rules used:
   --   - compound: 2 fields
-}


type alias Slot =
    { start : RosterTime
    , end : RosterTime
    , day : String
    , id : UUID
    }



-- interp. each slot has a number, is the same length, and none overlap
--         it can be thougth as an ID.
--
-- slot = Slot 1300000 1500000 "Monday" 1234
{-
    fnForSlot : Slot -> ...
    fnForSlot slot =
      ... slot.start  -- Time.Posix
          slot.end    -- Time.Posix
          slot.day    -- String
          slot.id     -- UUID
    -- Template rules used:
    --   - compound: 4 fields

   fnForListOfSlots : List Slot -> ...
   fnForListOfSlots  slots =
     case slots of
       [] -> []
       first :: rest -> ... (fnForSlot first)
                            (fnForListOfSlots rest)
-}


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


type Page
    = Home
    | Slots
    | Tas
    | Schedule


type alias SlotForm =
    { from : Int
    , to : Int
    , id : Int
    }


initialSlotForm : SlotForm
initialSlotForm =
    SlotForm 0 0 0


type alias Model =
    { key : Nav.Key
    , url : Url.Url -- I may don't need URL in the model
    , page : Maybe Page
    , slotForm : SlotForm
    , slots : List SlotForm
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url (getPage url) initialSlotForm [], Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateSlotForm SlotForm
    | UpdateSlots


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, page = getPage url }, Cmd.none )

        UpdateSlotForm slotForm ->
            ( { model | slotForm = slotForm }, Cmd.none )

        UpdateSlots ->
            let
                slotForm =
                    SlotForm model.slotForm.from model.slotForm.to 0
            in
            ( { model
                | slots = slotForm :: model.slots
                , slotForm = initialSlotForm
              }
            , Cmd.none
            )


route : Parser (Page -> a) a
route =
    oneOf
        [ map Home top
        , map Slots (s "slots")
        , map Tas (s "tas")
        , map Schedule (s "schedule")
        ]


getPage : Url.Url -> Maybe Page
getPage =
    parse route



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , div [ id "nav" ]
            [ ul []
                [ viewLink "/" "Home"
                , viewLink "/slots" "Slots"
                , viewLink "/tas" "Tas"
                , viewLink "/schedule" "Schedule"
                ]
            ]
        , div [ id "body" ]
            [ case model.page of
                Nothing ->
                    h2 [] [ text "Not Found" ]

                Just page ->
                    case page of
                        Home ->
                            h2 [] [ text "Home" ]

                        Slots ->
                            div [ class "content" ]
                                [ h2 [] [ text "Slots" ]
                                , div [ class "slots" ] <| List.map (\slot -> text <| String.fromInt slot.from) model.slots
                                , slotFormView model.slotForm
                                ]

                        Tas ->
                            h2 [] [ text "Tas" ]

                        Schedule ->
                            h2 [] [ text "Schedule" ]
            ]
        ]
    }


viewLink : String -> String -> Html msg
viewLink path name =
    li [] [ a [ href path ] [ text name ] ]


slotFormView : SlotForm -> Html Msg
slotFormView slotForm =
    Html.form [ onSubmit UpdateSlots ]
        [ label []
            [ text "Start at"
            , input
                [ type_ "text"
                , placeholder "Start at"
                , value <| String.fromInt slotForm.from
                , onInput (\s -> UpdateSlotForm { slotForm | from = stringToInt s })
                ]
                []
            ]
        , label []
            [ text "End at"
            , input
                [ type_ "text"
                , placeholder "End at"
                , value <| String.fromInt slotForm.to
                , onInput (\s -> UpdateSlotForm { slotForm | to = stringToInt s })
                ]
                []
            ]
        , button [ type_ "submit" ] [ text "Add" ]
        ]



-- A helper for string to int


stringToInt : String -> Int
stringToInt s =
    case String.toInt s of
        Nothing ->
            0

        Just i ->
            i



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
