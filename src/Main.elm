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
import Json.Decode as Decode
import Json.Encode as Encode exposing (encode)
import List.Extra
import Player exposing (Player)
import Random exposing (Generator)
import Task


type alias Flags =
    ()


type alias Model =
    { players : Dict String Player

    -- view state: what match are we playing now?
    , currentMatch : Maybe ( Player, Player )

    -- view state: new player form
    , newPlayerName : String
    }


type Msg
    = KeeperUpdatedNewPlayerName String
    | KeeperWantsToAddNewPlayer
    | KeeperWantsToRetirePlayer Player
    | StartMatchBetween ( Player, Player )
    | MatchFinished Player Elo.Outcome Player
    | KeeperWantsToSaveStandings
    | KeeperWantsToLoadStandings
    | SelectedStandingsFile File
    | LoadedStandings (Result String (Dict String Player))


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { players =
            List.range 49 122
                |> List.map Char.fromCode
                |> List.map String.fromChar
                |> List.map (\c -> ( c, Player c 1200 0 ))
                |> Dict.fromList
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
                | players = Dict.insert model.newPlayerName (Player.init model.newPlayerName) model.players
                , newPlayerName = ""
              }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        KeeperWantsToRetirePlayer player ->
            ( { model
                | players = Dict.remove player.name model.players
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

        StartMatchBetween players ->
            ( { model | currentMatch = Just players }
            , Cmd.none
            )

        MatchFinished playerA outcome playerB ->
            let
                ( playerARating, playerBRating ) =
                    newRating playerA.rating outcome playerB.rating
            in
            ( { model
                | players =
                    model.players
                        |> Dict.update playerA.name (Maybe.map (Player.incrementMatchesPlayed >> Player.setRating playerARating))
                        |> Dict.update playerB.name (Maybe.map (Player.incrementMatchesPlayed >> Player.setRating playerBRating))
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
                (encode 2 (Encode.dict identity Player.encode model.players))
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
                        case Decode.decodeString (Decode.dict Player.decoder) jsonString of
                            Ok decoded ->
                                Task.succeed decoded

                            Err err ->
                                Task.fail (Decode.errorToString err)
                    )
                |> Task.attempt LoadedStandings
            )

        LoadedStandings (Ok players) ->
            ( { model
                | players = players
                , currentMatch = Nothing
              }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        LoadedStandings (Err problem) ->
            let
                _ =
                    Debug.log "problem" problem
            in
            ( model, Cmd.none )


startNextMatchIfPossible : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startNextMatchIfPossible ( model, cmd ) =
    if model.currentMatch /= Nothing then
        -- there's a match already in progress; no need to overwrite it.
        ( model, cmd )

    else
        let
            players =
                Dict.values model.players
        in
        case players of
            first :: next :: rest ->
                ( model
                , Cmd.batch
                    [ cmd
                    , Random.generate StartMatchBetween (match first next rest)
                    ]
                )

            _ ->
                ( model, cmd )


openSans : Css.Style
openSans =
    Css.fontFamilies [ "'Open Sans'", "sans-serif" ]


{-| We need at least two players to guarantee that we return two distinct
players.
-}
match : Player -> Player -> List Player -> Generator ( Player, Player )
match a b rest =
    let
        allPlayers =
            a :: b :: rest

        minimumMatches =
            allPlayers
                |> List.map .matches
                |> List.minimum
                |> Maybe.withDefault 0

        leastPlayed =
            allPlayers
                |> List.filter (\player -> player.matches == minimumMatches)
    in
    allPlayers
        |> List.Extra.uniquePairs
        |> List.filter (\( left, right ) -> List.member left leastPlayed || List.member right leastPlayed)
        |> List.map
            (\( left, right ) ->
                ( toFloat <| abs (left.rating - right.rating)
                , ( left, right )
                )
            )
        |> -- flip the ordering so that the smallest gap / match adjustment is the most
           -- likely to be picked.
           (\weights ->
                let
                    maxDiff =
                        List.maximum (List.map Tuple.first weights) |> Maybe.withDefault (10 ^ 9)
                in
                List.map (\( diff, pair ) -> ( (maxDiff - diff) ^ 2, pair )) weights
           )
        |> (\weights ->
                case weights of
                    firstWeight :: restOfWeights ->
                        Random.weighted firstWeight restOfWeights

                    _ ->
                        -- how did we get here? Unless... a and b were the same
                        -- player? Sneaky caller!
                        Random.constant ( a, b )
           )


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
                [ rankings (Dict.values model.players)
                , newPlayerForm model
                , Html.button [ Events.onClick KeeperWantsToSaveStandings ] [ Html.text "Save Standings" ]
                , Html.button [ Events.onClick KeeperWantsToLoadStandings ] [ Html.text "Load Standings" ]
                , currentMatch model
                ]
            ]
        ]
            |> List.map Html.toUnstyled
    }


currentMatch : Model -> Html Msg
currentMatch model =
    case model.currentMatch of
        Just ( playerA, playerB ) ->
            let
                chanceAWins =
                    Elo.odds playerA.rating playerB.rating

                chanceBWins =
                    1 - chanceAWins

                ( ratingAWins, _ ) =
                    newRating playerA.rating Elo.WonAgainst playerB.rating

                ( ratingBWins, _ ) =
                    newRating playerB.rating Elo.WonAgainst playerA.rating

                upsideA =
                    ratingAWins - playerA.rating

                upsideB =
                    ratingBWins - playerB.rating
            in
            Html.section
                [ css
                    [ Css.maxWidth (Css.px 1024)
                    , Css.margin2 Css.zero Css.auto
                    ]
                ]
                [ Html.div
                    [ css [ Css.backgroundColor (Css.hex "#EEE"), Css.displayFlex ] ]
                    [ Html.p
                        [ css
                            [ Css.flexGrow (Css.num chanceAWins)
                            , Css.paddingRight (Css.px 5)
                            , Css.textAlign Css.right
                            , Css.backgroundColor (Css.hex "#1E90FF")
                            , Css.lineHeight (Css.px 50)
                            , Css.margin Css.zero
                            ]
                        ]
                        [ Html.text (percent chanceAWins) ]
                    , Html.p
                        [ css
                            [ Css.flexGrow (Css.num (1 - chanceAWins))
                            , Css.paddingLeft (Css.px 5)
                            , Css.lineHeight (Css.px 50)
                            , Css.margin Css.zero
                            ]
                        ]
                        [ Html.text (percent (1 - chanceAWins)) ]
                    ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.justifyContent Css.spaceAround
                        , Css.width (Css.pct 100)
                        ]
                    ]
                    [ activePlayer chanceAWins playerA upsideA (MatchFinished playerA Elo.WonAgainst playerB)
                    , Html.div []
                        [ Html.p [] [ Html.text "vs." ]
                        , Html.button [ Events.onClick (MatchFinished playerA Elo.DrewWith playerB) ] [ Html.text "It's a tie!" ]
                        ]
                    , activePlayer (1 - chanceAWins) playerB upsideB (MatchFinished playerB Elo.WonAgainst playerA)
                    ]
                ]

        Nothing ->
            Html.text "no match right now... add some players, maybe?"


activePlayer : Float -> Player -> Int -> Msg -> Html Msg
activePlayer chanceToWin player upside winMsg =
    Html.div []
        [ Html.h2 [] [ Html.text player.name ]
        , Html.p []
            [ Html.text (String.fromInt player.rating)
            , Html.text " after "
            , Html.text (String.fromInt player.matches)
            , Html.text " matches."
            ]
        , Html.p []
            [ Html.text "Stands to gain "
            , Html.text (String.fromInt upside)
            , Html.text " points."
            ]
        , Html.button
            [ Events.onClick winMsg ]
            [ Html.text (player.name ++ " wins!") ]
        ]


newRating : Int -> Elo.Outcome -> Int -> ( Int, Int )
newRating =
    Elo.newRating Elo.sensitiveKFactor


percent : Float -> String
percent chanceToWin =
    (toFloat (round (chanceToWin * 10000)) / 100 |> String.fromFloat) ++ "%"


rankings : List Player -> Html Msg
rankings players =
    let
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
    players
        |> List.sortBy (\player -> -player.rating)
        |> List.indexedMap
            (\rank player ->
                Html.tr
                    [ css [ Css.height (Css.px 60) ] ]
                    [ Html.td
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
                        [ Html.button
                            [ Events.onClick (KeeperWantsToRetirePlayer player) ]
                            [ Html.text "Retire" ]
                        ]
                    ]
            )
        |> (::)
            (Html.tr
                [ css [ Css.height (Css.px 45) ] ]
                [ Html.th [ css [ header, center ] ] [ Html.text "Rank" ]
                , Html.th [ css [ header, center ] ] [ Html.text "Rating" ]
                , Html.th [ css [ header, center ] ] [ Html.text "Matches" ]
                , Html.th [ css [ header, left ] ] [ Html.text "Name" ]
                , Html.th [ css [ header, center ] ] [ Html.text "Actions" ]
                ]
            )
        |> Html.table
            [ css
                [ Css.width (Css.pct 80)
                , Css.margin2 Css.zero Css.auto
                , Css.borderCollapse Css.collapse
                ]
            ]


newPlayerForm : { whatever | newPlayerName : String } -> Html Msg
newPlayerForm form =
    WildWildHtml.form
        [ Events.onSubmit KeeperWantsToAddNewPlayer ]
        [ Html.labelBefore
            []
            (Html.text "Player Name:")
            (Html.inputText form.newPlayerName [ Events.onInput KeeperUpdatedNewPlayerName ])
        , Html.button [] [ Html.text "Add Player" ]
        ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
