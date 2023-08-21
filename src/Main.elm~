module Main exposing (..)

import Browser
import Dict
import Element
import Element.Input as Input
import EventList exposing (..)
import Html exposing (Html, br, button, div, h1, h2, h3, input, span, text)
import Html.Attributes exposing (checked, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import Random.List
import Types exposing (..)
import Util exposing (..)


init : ( Model, Cmd Msg )
init =
    ( Setup (SetupModel [] 7), Cmd.none )


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = subs
        }


deckGen : Int -> List Event -> Random.Generator (List Event)
deckGen n possibleEvents =
    Random.List.choices n possibleEvents
        |> Random.map Tuple.first
        |> Random.andThen Random.List.shuffle


getUnits : List Unit -> List Event
getUnits units =
    units
        |> List.map unitToStr
        |> List.map (\x -> Dict.get x unitDict)
        |> List.map (Maybe.withDefault [])
        |> List.concat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if msg == Start then
        case model of
            Setup m ->
                ( Play (PlayModel [] (Event "" 0 "") [] 0)
                , Random.generate HaveDeck (deckGen (m.size + 2) (getUnits m.units))
                )

            _ ->
                init

    else
        ( case model of
            Setup m ->
                updateSetup msg m

            Play m ->
                updatePlay msg m

            -- MoreInfo, Wrong, and Ended are all static pages that you return to a diff one
            MoreInfo _ m ->
                m

            Wrong _ _ _ m ->
                m

            -- The only place to go from ended is reset
            Ended s t ->
                updateEnded msg s t
        , Cmd.none
        )


updateEnded : Msg -> Int -> List Event -> Model
updateEnded msg s t =
    case msg of
        LearnMore e ->
            MoreInfo e (Ended s t)

        _ ->
            Setup (SetupModel [] 7)


updateSetup : Msg -> SetupModel -> Model
updateSetup msg model =
    case msg of
        Select u ->
            if List.member u model.units then
                Setup { model | units = listRemove u model.units }

            else
                Setup { model | units = listInsert u model.units }

        ChangeNum s ->
            Setup
                { model
                    | size =
                        Maybe.withDefault
                            (if s /= "" then
                                model.size

                             else
                                0
                            )
                            (String.toInt s)
                }

        Start ->
            testStart

        _ ->
            Setup model


testStart : Model
testStart =
    Play
        (PlayModel
            [ Event "2" 20 "2"
            , Event "3" 30 "3"
            ]
            (Event "test 1" 10 "test 1 event")
            [ Event "test 0" 0 "test 0 event" ]
            0
        )


updatePlay : Msg -> PlayModel -> Model
updatePlay msg model =
    case msg of
        HaveDeck (first :: second :: rest) ->
            Play (PlayModel rest first [second] 0)

        HaveDeck notLong ->
            Setup (SetupModel [] 7)
        
        Guess d1 d2 ->
            case model.deck of
                [] ->
                    if guessCorrect model.active.date d1 d2 then
                        Ended (model.score + 1) (model.active :: model.played)

                    else
                        Wrong model.active d1 d2 (Ended model.score (model.active :: model.played))

                head :: tail ->
                    if guessCorrect model.active.date d1 d2 then
                        Play
                            { model
                                | deck = tail
                                , active = head
                                , played = model.active :: model.played
                                , score = model.score + 1
                            }

                    else
                        Wrong model.active
                            d1
                            d2
                            (Play
                                { model
                                    | deck = tail
                                    , active = head
                                    , played = model.active :: model.played
                                }
                            )

        LearnMore e ->
            MoreInfo e (Play model)

        _ ->
            Play model


updateMoreInfo : Msg -> Model -> Model
updateMoreInfo msg model =
    model


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Timeline"
        [ case model of
            Setup m ->
                viewSetup m

            Play m ->
                viewPlay m

            MoreInfo e m ->
                viewMoreInfo e

            Wrong e d1 d2 m ->
                viewWrong e d1 d2

            Ended s l ->
                viewEnded s l
        ]


viewSetup : SetupModel -> Html Msg
viewSetup model =
    div []
        [ h1 [] [ text "Welcome to timeline!" ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "justify-content" "space-around"
            ]
            [ div []
                [ h2 [] [ text "Select units to study" ]
                , div [] (List.map (viewUnitSelect model.units) unitList)
                ]
            , div []
                [ h2 [] [ text "Select number of cards to study" ]
                , button [ onClick (ChangeNum (String.fromInt (model.size - 1))) ] [ text "-" ]
                , input [ onInput ChangeNum, value (String.fromInt model.size) ] []
                , button [ onClick (ChangeNum (String.fromInt (model.size + 1))) ] [ text "+" ]
                ]
            , button [ onClick Start ] [ h3 [] [ text "Start!" ] ]
            ]
        ]


viewUnitSelect : List Unit -> Unit -> Html Msg
viewUnitSelect mod unit =
    div []
        [ input
            [ onClick (Select unit)
            , type_ "checkbox"
            , checked (List.member unit mod)
            ]
            []
        , text ("Unit " ++ unitToStr unit)
        ]


viewPlay : PlayModel -> Html Msg
viewPlay model =
    div []
        [ h1 [] [ text "Timeline" ]
        , span []
            [ text "event: "
            , span [ style "text-decoration" "underline" ] [ text model.active.name ]
            , button [ onClick (LearnMore model.active) ] [ text "learn more" ]
            ]
        , viewTimeline (List.sortBy .date model.played)
        ]


viewTimeline : List Event -> Html Msg
viewTimeline listEvents =
    let
        leftDates : List (Maybe Int)
        leftDates =
            Nothing :: List.map (.date >> Just) listEvents

        rightDates : List (Maybe Int)
        rightDates =
            List.append (List.map (.date >> Just) listEvents) [ Nothing ]

        selectionButtons : List (Html Msg)
        selectionButtons =
            List.map2 (\r l -> button [ onClick (Guess r l) ] [ text "here" ])
                leftDates
                rightDates

        placedDates : List (Html Msg)
        placedDates =
            List.map
                (\e ->
                    div []
                        [ h3 [] [ text e.name ]
                        , div [] [ text (String.fromInt e.date) ]
                        , button [ onClick (LearnMore e) ] [ text "learn more" ]
                        ]
                )
                listEvents
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-around"
        ]
        (listInterleave
            selectionButtons
            placedDates
        )


viewMoreInfo : Event -> Html Msg
viewMoreInfo event =
    div []
        [ h1 [] [ text event.name ]
        , span [] [ text event.desc ]
        , button [ onClick Back ] [ text "back" ]
        ]


viewWrong : Event -> Maybe Int -> Maybe Int -> Html Msg
viewWrong e d1 d2 =
    div []
        [ h2 [] [ text "sorry, that's incorrect!" ]
        , div []
            [ text
                ("you guessed "
                    ++ e.name
                    ++ " occured "
                    ++ betweenStr d1 d2
                    ++ "; it occured in "
                    ++ String.fromInt e.date
                )
            ]
        , div [] [ text e.desc ]
        , button [ onClick Back ] [ text "back" ]
        ]


viewEnded : Int -> List Event -> Html Msg
viewEnded i listEvents =
    div []
        [ h3
            []
            [ text
                ("You got "
                    ++ String.fromInt i
                    ++ "/"
                    ++ String.fromInt (List.length listEvents - 1)
                )
            ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "row"

            --, style "justify-content" "space-around"
            ]
            (List.map
                (\e ->
                    div [ style "border-style" "solid" ]
                        [ h3 [] [ text e.name ]
                        , div [] [ text (String.fromInt e.date) ]
                        , button [ onClick (LearnMore e) ] [ text "learn more" ]
                        ]
                )
                (List.sortBy .date listEvents)
            )
        , button [ onClick Back ] [ text "restart" ]
        ]


subs _ =
    Sub.none
