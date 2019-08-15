module Main exposing (Assignment, Schedule, Slot, TA, addSlotFromId, scheduleTas)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Random exposing (generate)
import UUID exposing (..)
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


type alias Slot =
    { start : String
    , end : String
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
    { start : String
    , end : String
    , day : String
    }


initialSlotForm : SlotForm
initialSlotForm =
    SlotForm "00:00" "00:00" "Monday"


type alias TaForm =
    TA


initialTaForm : TaForm
initialTaForm =
    TA "" 0 []


type alias Model =
    { key : Nav.Key
    , url : Url.Url -- I may don't need URL in the model
    , page : Maybe Page
    , slotForm : SlotForm
    , slots : List Slot
    , taForm : TaForm
    , tas : List TA
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url (getPage url) initialSlotForm [] initialTaForm [], Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateSlotForm SlotForm
    | UpdateSlots
    | GenerateUUID UUID.UUID
    | UpdateTaForm TaForm
    | UpdateTas


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
            ( model, Random.generate GenerateUUID UUID.generator )

        GenerateUUID uuid ->
            let
                slot =
                    Slot model.slotForm.start model.slotForm.end model.slotForm.day uuid
            in
            ( { model | slots = slot :: model.slots, slotForm = initialSlotForm }
            , Cmd.none
            )

        UpdateTaForm taForm ->
            ( { model | taForm = taForm }, Cmd.none )

        UpdateTas ->
            ( { model | tas = model.taForm :: model.tas, taForm = initialTaForm }, Cmd.none )


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
        , div [ id "body" ] <|
            case model.page of
                Nothing ->
                    [ h2 [] [ text "Not Found" ] ]

                Just page ->
                    case page of
                        Home ->
                            [ h2 [] [ text "Home" ] ]

                        Slots ->
                            [ h2 [] [ text "Slots" ]
                            , slotsTable model.slots
                            , slotFormView model.slotForm
                            ]

                        Tas ->
                            [ h2 [] [ text "Tas" ]
                            , tasTable model.tas
                            , tasFormView model.taForm model.slots
                            ]

                        Schedule ->
                            [ h2 [] [ text "Schedule" ] ]
        ]
    }


viewLink : String -> String -> Html msg
viewLink path name =
    li [] [ a [ href path ] [ text name ] ]


slotsTable : List Slot -> Html Msg
slotsTable slots =
    let
        slotRow slot =
            tr []
                [ td [] [ text slot.day ]
                , td [] [ text slot.start ]
                , td [] [ text slot.end ]
                ]
    in
    table [] <|
        List.map slotRow slots


slotFormView : SlotForm -> Html Msg
slotFormView slotForm =
    Html.form [ onSubmit UpdateSlots ]
        [ label []
            [ text "Day"
            , select
                [ placeholder "Day"
                , value slotForm.day
                , onInput (\s -> UpdateSlotForm { slotForm | day = s })
                ]
                dayOptions
            ]
        , label []
            [ text "Start at"
            , select
                [ placeholder "Start at"
                , value slotForm.start
                , onInput (\s -> UpdateSlotForm { slotForm | start = s })
                ]
                timeOptions
            ]
        , label []
            [ text "End at"
            , select
                [ placeholder "End at"
                , value slotForm.end
                , onInput (\s -> UpdateSlotForm { slotForm | end = s })
                ]
                timeOptions
            ]
        , button [ type_ "submit" ] [ text "Add" ]
        ]


timeOptions =
    [ option [ value "00:00" ] [ text "00:00" ]
    , option [ value "01:00" ] [ text "01:00" ]
    , option [ value "02:00" ] [ text "02:00" ]
    , option [ value "03:00" ] [ text "03:00" ]
    , option [ value "04:00" ] [ text "04:00" ]
    , option [ value "05:00" ] [ text "05:00" ]
    , option [ value "06:00" ] [ text "06:00" ]
    , option [ value "07:00" ] [ text "07:00" ]
    , option [ value "08:00" ] [ text "08:00" ]
    , option [ value "09:00" ] [ text "09:00" ]
    , option [ value "10:00" ] [ text "10:00" ]
    , option [ value "11:00" ] [ text "11:00" ]
    , option [ value "12:00" ] [ text "12:00" ]
    , option [ value "13:00" ] [ text "13:00" ]
    , option [ value "14:00" ] [ text "14:00" ]
    , option [ value "15:00" ] [ text "15:00" ]
    , option [ value "16:00" ] [ text "16:00" ]
    , option [ value "17:00" ] [ text "17:00" ]
    , option [ value "18:00" ] [ text "18:00" ]
    , option [ value "19:00" ] [ text "19:00" ]
    , option [ value "20:00" ] [ text "20:00" ]
    , option [ value "21:00" ] [ text "21:00" ]
    , option [ value "22:00" ] [ text "22:00" ]
    , option [ value "23:00" ] [ text "23:00" ]
    ]


dayOptions =
    [ option [ value "Monday" ] [ text "Monday" ]
    , option [ value "Tuesday" ] [ text "Tuesday" ]
    , option [ value "Wednesday" ] [ text "Wednesday" ]
    , option [ value "Thursday" ] [ text "Thrusday" ]
    , option [ value "Friday" ] [ text "Friday" ]
    , option [ value "Saturday" ] [ text "Saturday" ]
    , option [ value "Sunday" ] [ text "Sunday" ]
    ]


tasTable : List TA -> Html Msg
tasTable tas =
    let
        taRow ta =
            tr []
                [ td [] [ text ta.name ]
                , td [] [ text (String.fromInt ta.max) ]
                , td [] [ text (String.fromInt <| List.length ta.avail) ]
                ]
    in
    table [] <| List.map taRow tas


tasFormView : TaForm -> List Slot -> Html Msg
tasFormView taForm slots =
    Html.form [ onSubmit UpdateTas ]
        [ label []
            [ text "Name"
            , input
                [ type_ "text"
                , placeholder "Name"
                , value taForm.name
                , onInput (\t -> UpdateTaForm { taForm | name = t })
                ]
                []
            ]
        , label []
            [ text "Max."
            , input
                [ type_ "text"
                , placeholder "Max. number of slots"
                , value (String.fromInt taForm.max)
                , onInput (\t -> UpdateTaForm { taForm | max = stringToInt t })
                ]
                []
            ]
        , label []
            [ text "Availability"
            , select
                [ multiple True
                , placeholder "Availability"
                , onInput (\t -> UpdateTaForm { taForm | avail = addSlotFromId slots taForm.avail t })
                ]
                (slotOptions slots)
            ]
        , button [ type_ "submit" ] [ text "Add" ]
        ]


slotOptions : List Slot -> List (Html Msg)
slotOptions slots =
    let
        slotToOption slot =
            option [ value (UUID.toString slot.id) ] [ text (slotToString slot) ]
    in
    List.map slotToOption slots


addSlotFromId : List Slot -> List Slot -> String -> List Slot
addSlotFromId slots avail id =
    let
        getMaybeSlot : List Slot -> String -> Maybe Slot
        getMaybeSlot los uuid =
            case los of
                [] ->
                    Nothing

                first :: rest ->
                    if UUID.toString first.id == uuid then
                        Just first

                    else
                        getMaybeSlot rest uuid

        maybeSlot =
            getMaybeSlot slots id
    in
    case maybeSlot of
        Nothing ->
            avail

        Just slot ->
            slot :: avail


slotToString : Slot -> String
slotToString slot =
    slot.day ++ ", " ++ slot.start ++ "-" ++ slot.end


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
