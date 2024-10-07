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
    ( Setup (SetupModel [] 7 False), Cmd.none )


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
                ( Play (PlayModel [] (Event "" 0 "") [] 0 m.hardMode)
                , Random.generate HaveDeck (deckGen (m.size + 1) (getUnits m.units))
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
            Setup (SetupModel [] 7 False)


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

        LearnMore e ->
            MoreInfo e (Setup model)

        ChangeHardMode ->
            Setup { model | hardMode = not model.hardMode }

        _ ->
            Setup model


updatePlay : Msg -> PlayModel -> Model
updatePlay msg model =
    case msg of
        HaveDeck (first :: second :: rest) ->
            Play (PlayModel rest first [ second ] 0 model.hardMode)

        HaveDeck notLong ->
            Setup (SetupModel [] 7 False)

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
        [ div
            [ style "margin" "20px"
            , style "top" "0"
            ]
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
            , div []
                [ case model of
                    MoreInfo _ _ ->
                        span [] []

                    Wrong _ _ _ _ ->
                        span [] []

                    _ ->
                        button
                            [ style "background-color" "rgb(130, 100, 130)"
                            , style "color" "white"
                            , style "border" "none"
                            , style "height" "45px"
                            , style "width" "100px"
                            , style "text-align" "center"
                            , style "vertical-align" "middle"
                            , style "text-decoration" "none"
                            , style "border-radius" "10px"
                            , onClick (LearnMore (Event "About Timeline" 0 "Timeline was created by Tobit Glenhaber to help students study for the APUSH exam (or just to learn US History). The code is under a MIT License and is available here: https://github.com/tglenhab/timeline"))
                            ]
                            [ text "More info" ]
                ]
            ]
        ]


viewSetup : SetupModel -> Html Msg
viewSetup model =
    div []
        [ div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "justify-content" "center"
            ]
            [ h1 [ style "font-weight" "900" ] [ text "Welcome to timeline!" ]
            ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "justify-content" "center"
            , style "gap" "5%"
            ]
            [ div []
                [ h2 [] [ text "Select units to study" ]
                , div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "justify-content" "center"
                    ]
                    (List.map (viewUnitSelect model.units) unitList)
                ]
            , div []
                [ h2 [] [ text "Select number of cards to study" ]
                , div
                    [ style "display" "flex"
                    , style "flex-direction" "row"
                    , style "justify-content" "center"
                    ]
                    [ button
                        [ style "background-color" "#A0B2C4"
                        , style "color" "white"
                        , style "text-align" "center"
                        , style "border" "none"
                        , style "height" "25px"
                        , style "width" "25px"
                        , style "margin" "5px"
                        , style "text-align" "center"
                        , style "text-decoration" "none"
                        , style "display" "inline-block"
                        , style "font-size" "16px"
                        , style "border-radius" "15%"
                        , style "cursor" "pointer"
                        , style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)"
                        , onClick (ChangeNum (String.fromInt (model.size - 1)))
                        ]
                        [ text "-" ]
                    , input
                        [ style "background-color" "#A0B2C4"
                        , style "color" "white"
                        , style "text-align" "center"
                        , style "border" "none"
                        , style "height" "25px"
                        , style "width" "25px"
                        , style "margin" "5px"
                        , style "text-align" "center"
                        , style "text-decoration" "none"
                        , style "display" "inline-block"
                        , style "font-size" "16px"
                        , style "border-radius" "15%"
                        , style "cursor" "pointer"
                        , style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)"
                        , onInput ChangeNum
                        , value (String.fromInt model.size)
                        ]
                        []
                    , button
                        [ style "background-color" "#A0B2C4"
                        , style "color" "white"
                        , style "border" "none"
                        , style "height" "25px"
                        , style "width" "25px"
                        , style "margin" "5px"
                        , style "text-align" "center"
                        , style "text-align" "center"
                        , style "text-decoration" "none"
                        , style "display" "inline-block"
                        , style "font-size" "16px"
                        , style "border-radius" "15%"
                        , style "cursor" "pointer"
                        , style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)"
                        , onClick (ChangeNum (String.fromInt (model.size + 1)))
                        ]
                        [ text "+" ]
                    ]
                ]
            , div []
                [ div []
                    [ h2 [] [ text "Enable Hard Mode" ]
                    , div
                        [ style "display" "flex"
                        , style "flex-direction" "row"
                        , style "justify-content" "center"
                        ]
                        [ input [ onClick ChangeHardMode, type_ "checkbox", checked model.hardMode ] []
                        , text "Hard Mode"
                        , button
                            [ style "background-color" "#A0B2C4"
                            , style "color" "white"
                            , style "border" "none"
                            , style "border-radius" "50%"
                            , style "margin-left" "5px"
                            , onClick (LearnMore (Event "About Timeline" 0 "In Hard Mode, you cannot learn about events until they are on the board"))
                            ]
                            [ text "?" ]
                        ]
                    ]
                ]
            ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "justify-content" "center"
            ]
            [ button
                [ style "background-color" "rgb(10, 160, 70)"
                , style "color" "white"
                , style "border" "none"
                , style "padding" "0.8rem 5.2rem"
                , style "text-align" "center"
                , style "margin-top" "50px"
                , style "margin-bottom" "20px"
                , style "vertical-align" "middle"
                , style "text-decoration" "none"
                , style "border-radius" "10px"
                , style "font-size" "1.5rem"
                , onClick Start
                ]
                [ text "Start!" ]
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
        , text ("Unit " ++ unitToStr unit ++ unitPeriod unit)
        ]


viewPlay : PlayModel -> Html Msg
viewPlay model =
    div []
        [ h1 [] [ text "Timeline" ]
        , span []
            [ text "Event: "
            , span [ style "text-decoration" "underline" ] [ text model.active.name ]
            , if not model.hardMode then
                div []
                    [ button
                        [ style "background-color" "rgb(10, 100, 240)"
                        , style "color" "white"
                        , style "border" "none"
                        , style "padding" "0.4rem 0.4rem"
                        , style "margin" "2px"
                        , style "text-align" "center"
                        , style "vertical-align" "middle"
                        , style "text-decoration" "none"
                        , style "border-radius" "4px"
                        , style "font-size" "1rem"
                        , onClick (LearnMore model.active)
                        ]
                        [ text "learn more" ]
                    ]

              else
                span [] []
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
                    div
                        [ style "display" "flex"
                        , style "flex-direction" "column"
                        , style "justify-content" "space-around"
                        , style "align-items" "center"
                        , style "padding" "10px"
                        , style "margin" "10px"
                        , style "border-style" "solid"
                        ]
                        [ h3 [] [ text e.name ]
                        , div [] [ text (String.fromInt e.date) ]
                        , button [ onClick (LearnMore e) ] [ text "learn more" ]
                        ]
                )
                listEvents
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "space-around"
        , style "align-items" "center"

        --, style "flex-wrap" "wrap"
        ]
        (listInterleave
            selectionButtons
            placedDates
        )


viewMoreInfo : Event -> Html Msg
viewMoreInfo event =
    div []
        [ h1 [] [ text event.name ]
        , div [] [ text event.desc ]
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
        , div [] [ text "-----" ]
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
