module Main exposing (..)

import Accessibility.Styled as Html exposing (Html)
import Browser exposing (Document)
import Css
import Css.Reset
import Dict exposing (Dict)
import Elo
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html.Styled as WildWildHtml
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode exposing (encode)
import Keyboard
import League exposing (League)
import Player exposing (Player)
import Random
import Task


type alias Flags =
    ()


type alias Model =
    { league : League
    , leagueBeforeLastMatch : League

    -- view state: what match are we playing now?
    , currentMatch : Maybe ( Player, Player )

    -- view state: new player form
    , newPlayerName : String
    }


type Msg
    = KeeperUpdatedNewPlayerName String
    | KeeperWantsToAddNewPlayer
    | KeeperWantsToRetirePlayer Player
    | StartMatch (Maybe ( Player, Player ))
    | MatchFinished Player Elo.Outcome Player
    | KeeperWantsToSaveStandings
    | KeeperWantsToLoadStandings
    | SelectedStandingsFile File
    | LoadedLeague (Result String League)
    | IgnoredKey


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { league = League.init
      , leagueBeforeLastMatch = League.init
      , currentMatch = Nothing
      , newPlayerName = ""
      }
    , Cmd.none
    )
        |> startNextMatchIfPossible


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeeperUpdatedNewPlayerName newPlayerName ->
            ( { model | newPlayerName = newPlayerName }
            , Cmd.none
            )

        KeeperWantsToAddNewPlayer ->
            ( { model
                | league = League.addNewPlayer (Player.init model.newPlayerName) model.league
                , leagueBeforeLastMatch = League.addNewPlayer (Player.init model.newPlayerName) model.leagueBeforeLastMatch
                , newPlayerName = ""
              }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        KeeperWantsToRetirePlayer player ->
            ( { model
                | league = League.retirePlayer player model.league
                , leagueBeforeLastMatch = League.retirePlayer player model.leagueBeforeLastMatch
                , currentMatch =
                    case model.currentMatch of
                        Nothing ->
                            Nothing

                        Just ( a, b ) ->
                            if player == a || player == b then
                                Nothing

                            else
                                Just ( a, b )
              }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        StartMatch (Just match) ->
            ( { model | currentMatch = Just match }
            , Cmd.none
            )

        StartMatch Nothing ->
            ( model, Cmd.none )

        MatchFinished playerA outcome playerB ->
            let
                ( playerARating, playerBRating ) =
                    newRating playerA.rating outcome playerB.rating
            in
            ( { model
                | league =
                    model.league
                        |> League.updatePlayer (Player.incrementMatchesPlayed (Player.setRating playerARating playerA))
                        |> League.updatePlayer (Player.incrementMatchesPlayed (Player.setRating playerBRating playerB))
                , leagueBeforeLastMatch = model.league
                , currentMatch = Nothing
              }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        KeeperWantsToSaveStandings ->
            ( model
            , Download.string
                "standings.json"
                "application/json"
                (encode 2 (League.encode model.league))
            )

        KeeperWantsToLoadStandings ->
            ( model
            , Select.file [ "application/json" ] SelectedStandingsFile
            )

        SelectedStandingsFile file ->
            ( model
            , File.toString file
                |> Task.andThen
                    (\jsonString ->
                        case Decode.decodeString League.decoder jsonString of
                            Ok decoded ->
                                Task.succeed decoded

                            Err err ->
                                Task.fail (Decode.errorToString err)
                    )
                |> Task.attempt LoadedLeague
            )

        LoadedLeague (Ok league) ->
            ( { model
                | league = league
                , leagueBeforeLastMatch = league
                , currentMatch = Nothing
              }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        LoadedLeague (Err problem) ->
            -- TODO: show a problem
            ( model, Cmd.none )

        IgnoredKey ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentMatch of
        Just ( left, right ) ->
            Keyboard.downs
                (\rawKey ->
                    case Keyboard.navigationKey rawKey of
                        Just Keyboard.ArrowLeft ->
                            MatchFinished left Elo.WonAgainst right

                        Just Keyboard.ArrowRight ->
                            MatchFinished right Elo.WonAgainst left

                        Just Keyboard.ArrowDown ->
                            MatchFinished left Elo.DrewWith right

                        _ ->
                            IgnoredKey
                )

        Nothing ->
            Sub.none


startNextMatchIfPossible : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startNextMatchIfPossible ( model, cmd ) =
    if model.currentMatch /= Nothing then
        -- there's a match already in progress; no need to overwrite it.
        ( model, cmd )

    else
        ( model
        , Cmd.batch
            [ cmd
            , Random.generate StartMatch (League.match model.league)
            ]
        )


openSans : Css.Style
openSans =
    Css.fontFamilies [ "'Open Sans'", "sans-serif" ]


view : Model -> Document Msg
view model =
    { title = "ELO Anything!"
    , body =
        [ Css.Reset.meyerV2
        , Css.Reset.borderBoxV201408
        , WildWildHtml.node "style" [] [ Html.text """
            @font-face {
                font-family: "Open Sans";
                src: url("/fonts/OpenSans-Regular-webfont.woff");
                font-weight: 500;
            }

            @font-face {
                font-family: "Open Sans";
                src: url("/fonts/OpenSans-Semibold-webfont.woff");
                font-weight: 600;
            }
          """ ]
        , Html.div [ css [ Css.width (Css.pct 100) ] ]
            [ Html.main_
                [ css
                    [ Css.maxWidth (Css.px 1024)
                    , Css.margin2 Css.zero Css.auto
                    ]
                ]
                [ currentMatch model
                , rankings model
                , Html.section
                    [ css [ Css.textAlign Css.center, Css.marginTop (Css.px 32) ] ]
                    [ blueButton "Save Standings" KeeperWantsToSaveStandings
                    , blueButton "Load Standings" KeeperWantsToLoadStandings
                    ]
                ]
            ]
        ]
            |> List.map Html.toUnstyled
    }


currentMatch : Model -> Html Msg
currentMatch model =
    case model.currentMatch of
        Nothing ->
            Html.div
                [ css
                    [ openSans
                    , Css.textAlign Css.center
                    , Css.width (Css.pct 50)
                    , Css.margin2 (Css.px 32) Css.auto
                    ]
                ]
                [ Html.h1
                    [ css
                        [ Css.fontSize (Css.px 32)
                        , Css.marginBottom (Css.px 18)
                        , Css.fontWeight (Css.int 600)
                        ]
                    ]
                    [ Html.text "Elo Anything" ]
                , Html.p
                    [ css
                        [ Css.fontSize (Css.px 24)
                        , Css.lineHeight (Css.px 32)
                        , Css.marginBottom (Css.px 18)
                        ]
                    ]
                    [ Html.text "No current match. To get started, add at least two players!" ]
                , blueButton "Load Standings" KeeperWantsToLoadStandings
                ]

        Just ( playerA, playerB ) ->
            let
                chanceAWins =
                    Elo.odds playerA.rating playerB.rating
            in
            Html.section
                [ css
                    [ Css.width (Css.pct 80)
                    , Css.margin2 (Css.px 32) Css.auto
                    ]
                ]
                [ Html.div
                    [ css
                        [ Css.borderRadius (Css.px 5)
                        , Css.overflow Css.hidden
                        , Css.height (Css.px 5)
                        , Css.width (Css.pct 100)
                        , Css.backgroundColor (Css.hex "EEE")
                        ]
                    ]
                    [ Html.div
                        [ css
                            [ Css.width (Css.pct (100 * chanceAWins))
                            , Css.height (Css.pct 100)
                            , Css.backgroundColor (Css.hex "6DD400")
                            ]
                        ]
                        []
                    ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.paddingTop (Css.px 32)
                        ]
                    ]
                    [ activePlayer playerA
                    , Html.p
                        [ css
                            [ openSans
                            , Css.width (Css.pct 20)
                            , Css.textAlign Css.center
                            ]
                        ]
                        [ Html.text "vs." ]
                    , activePlayer playerB
                    ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.paddingTop (Css.px 32)
                        , Css.textAlign Css.center
                        ]
                    ]
                    [ Html.div
                        [ css [ Css.width (Css.pct 40) ] ]
                        [ blueButton "Winner!" (MatchFinished playerA Elo.WonAgainst playerB) ]
                    , Html.div
                        [ css [ Css.width (Css.pct 20) ] ]
                        [ blueButton "Tie!" (MatchFinished playerA Elo.DrewWith playerB) ]
                    , Html.div
                        [ css [ Css.width (Css.pct 40) ] ]
                        [ blueButton "Winner!" (MatchFinished playerB Elo.WonAgainst playerA) ]
                    ]
                ]


button : Css.Color -> String -> Msg -> Html Msg
button baseColor label msg =
    Html.button
        [ css
            [ Css.paddingTop (Css.px 6)
            , Css.paddingBottom (Css.px 10)
            , Css.paddingLeft (Css.px 15)
            , Css.paddingRight (Css.px 15)
            , Css.minWidth (Css.px 100)
            , Css.backgroundColor baseColor
            , Css.border Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.boxShadow6 Css.inset Css.zero (Css.px -4) Css.zero Css.zero (Css.rgba 0 0 0 0.1)
            , Css.cursor Css.pointer

            -- font
            , Css.fontSize (Css.px 14)
            , Css.fontWeight (Css.int 600)
            , Css.color (Css.hex "FFF")
            ]
        , Events.onClick msg
        ]
        [ Html.text label ]


blueButton : String -> Msg -> Html Msg
blueButton =
    button (Css.hex "0091FF")


greenButton : String -> Msg -> Html Msg
greenButton =
    button (Css.hex "6DD400")


redButton : String -> Msg -> Html Msg
redButton =
    button (Css.hex "E02020")


activePlayer : Player -> Html msg
activePlayer player =
    Html.h2
        [ css
            [ Css.width (Css.pct 40)
            , Css.maxWidth (Css.pct 45)
            , Css.textAlign Css.center
            , Css.fontSize (Css.px 24)
            , openSans
            ]
        ]
        [ Html.text player.name ]


newRating : Int -> Elo.Outcome -> Int -> ( Int, Int )
newRating =
    Elo.newRating Elo.sensitiveKFactor


rankings : Model -> Html Msg
rankings model =
    let
        previousStandings =
            model.leagueBeforeLastMatch.players
                |> Dict.values
                |> List.sortBy (\player -> -player.rating)
                |> List.indexedMap (\rank player -> ( player.name, rank ))
                |> Dict.fromList

        numeric =
            Css.batch
                [ Css.fontWeight (Css.int 600)
                , Css.fontSize (Css.px 21)
                , Css.verticalAlign Css.middle
                , openSans
                ]

        textual =
            Css.batch
                [ Css.fontWeight (Css.int 500)
                , Css.fontSize (Css.px 18)
                , Css.lineHeight (Css.px 24)
                , Css.verticalAlign Css.middle
                , openSans
                , Css.paddingLeft (Css.px 15)
                ]

        shrinkWidth =
            Css.batch
                [ Css.paddingLeft (Css.px 15)
                , Css.paddingRight (Css.px 15)
                , Css.width (Css.pct 1)
                ]

        left =
            Css.textAlign Css.left

        center =
            Css.textAlign Css.center

        header =
            Css.batch
                [ Css.paddingRight (Css.px 15)
                , Css.paddingLeft (Css.px 15)
                , Css.verticalAlign Css.middle

                -- font
                , openSans
                , Css.fontWeight (Css.int 600)

                -- separators
                , Css.borderRight3 (Css.px 1) Css.solid (Css.hex "B1BECE")
                , Css.lastChild [ Css.borderRightWidth Css.zero ]
                ]
    in
    model.league.players
        |> Dict.values
        |> List.sortBy (\player -> -player.rating)
        |> List.indexedMap
            (\rank player ->
                let
                    previousRank =
                        Dict.get player.name previousStandings
                            |> Maybe.withDefault rank
                in
                ( player.name
                , Html.tr
                    [ css [ Css.height (Css.px 60) ] ]
                    [ Html.td
                        [ css
                            [ Css.verticalAlign Css.middle
                            , Css.textAlign Css.center
                            ]
                        ]
                        (if rank < previousRank then
                            [ upArrow (Css.hex "6DD400")
                            , Html.span
                                [ css
                                    [ openSans
                                    , Css.color (Css.hex "6DD400")
                                    , Css.fontSize (Css.px 14)
                                    ]
                                ]
                                [ Html.text (String.fromInt (previousRank - rank)) ]
                            ]

                         else if rank > previousRank then
                            [ downArrow (Css.hex "E02020")
                            , Html.span
                                [ css
                                    [ openSans
                                    , Css.color (Css.hex "E02020")
                                    , Css.fontSize (Css.px 14)
                                    ]
                                ]
                                [ Html.text (String.fromInt (abs (previousRank - rank))) ]
                            ]

                         else
                            [ Html.text "" ]
                        )
                    , Html.td
                        [ css [ numeric, shrinkWidth, center ] ]
                        [ Html.text (String.fromInt (rank + 1)) ]
                    , Html.td
                        [ css [ numeric, shrinkWidth, center ] ]
                        [ Html.text (String.fromInt player.rating) ]
                    , Html.td
                        [ css [ numeric, shrinkWidth, center ] ]
                        [ Html.text (String.fromInt player.matches) ]
                    , Html.td
                        [ css [ textual, left ] ]
                        [ Html.text player.name ]
                    , Html.td
                        [ css [ textual, shrinkWidth, center ] ]
                        [ redButton "Retire" (KeeperWantsToRetirePlayer player) ]
                    ]
                )
            )
        |> (::)
            ( "players-header"
            , Html.tr
                [ css [ Css.height (Css.px 45) ] ]
                [ Html.th [ css [ Css.width (Css.px 20) ] ] []
                , Html.th [ css [ header, center ] ] [ Html.text "Rank" ]
                , Html.th [ css [ header, center ] ] [ Html.text "Rating" ]
                , Html.th [ css [ header, center ] ] [ Html.text "Matches" ]
                , Html.th [ css [ header, left ] ] [ Html.text "Name" ]
                , Html.th [ css [ header, center ] ] [ Html.text "Actions" ]
                ]
            )
        |> (\tableGuts ->
                tableGuts
                    ++ [ ( "add-player-form"
                         , Html.tr
                            [ css [ Css.height (Css.px 60) ] ]
                            [ Html.td [] []
                            , Html.td
                                [ css [ numeric, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ]
                                [ Html.text "-" ]
                            , Html.td
                                [ css [ numeric, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ]
                                [ Html.text (String.fromInt Elo.initialRating) ]
                            , Html.td
                                [ css [ numeric, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ]
                                [ Html.text "0" ]
                            , Html.td
                                [ css [ textual, left ] ]
                                [ Html.inputText model.newPlayerName
                                    [ Events.onInput KeeperUpdatedNewPlayerName
                                    , Events.on "keydown"
                                        (Decode.field "key" Decode.string
                                            |> Decode.andThen
                                                (\key ->
                                                    case key of
                                                        "Enter" ->
                                                            Decode.succeed KeeperWantsToAddNewPlayer

                                                        _ ->
                                                            Decode.fail "ignoring"
                                                )
                                        )
                                    , css
                                        [ Css.border Css.zero
                                        , Css.fontSize (Css.px 18)
                                        , Css.padding2 (Css.px 5) (Css.px 15)
                                        , Css.width (Css.calc (Css.pct 100) Css.minus (Css.px 15))
                                        , Css.boxShadow6 Css.inset Css.zero (Css.px 1) (Css.px 2) Css.zero (Css.rgba 0 0 0 0.5)
                                        , Css.borderRadius (Css.px 5)
                                        ]
                                    ]
                                ]
                            , Html.td
                                [ css [ numeric, shrinkWidth, center ] ]
                                [ greenButton "Add" KeeperWantsToAddNewPlayer ]
                            ]
                         )
                       ]
           )
        |> Keyed.node "table"
            [ css
                [ Css.width (Css.pct 80)
                , Css.margin2 Css.zero Css.auto
                , Css.borderCollapse Css.collapse
                ]
            ]


upArrow : Css.Color -> Html msg
upArrow color =
    Html.div
        [ css
            [ Css.width Css.zero
            , Css.height Css.zero
            , Css.borderLeft3 (Css.px 5) Css.solid Css.transparent
            , Css.borderRight3 (Css.px 5) Css.solid Css.transparent
            , Css.borderBottom3 (Css.px 10) Css.solid color
            , Css.display Css.inlineBlock
            , Css.margin4 (Css.px 2) (Css.px 5) (Css.px 2) (Css.px 2)
            ]
        ]
        []


downArrow : Css.Color -> Html msg
downArrow color =
    Html.div
        [ css
            [ Css.width Css.zero
            , Css.height Css.zero
            , Css.borderLeft3 (Css.px 5) Css.solid Css.transparent
            , Css.borderRight3 (Css.px 5) Css.solid Css.transparent
            , Css.borderTop3 (Css.px 10) Css.solid color
            , Css.display Css.inlineBlock
            , Css.margin4 (Css.px 2) (Css.px 5) (Css.px 2) (Css.px 2)
            ]
        ]
        []


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
