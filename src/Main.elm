module Main exposing (..)

import Accessibility.Styled as Html exposing (Html)
import Browser exposing (Document)
import Dict exposing (Dict)
import Elo
import Html.Styled as WildWildHtml
import Html.Styled.Events as Events
import Player exposing (Player)


type alias Flags =
    ()


type alias Model =
    { players : Dict String Player

    -- view state: new player form
    , newPlayerName : String
    }


type Msg
    = KeeperUpdatedNewPlayerName String
    | KeeperWantsToAddNewPlayer


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { players = Dict.empty, newPlayerName = "" }, Cmd.none )


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


view : Model -> Document Msg
view model =
    { title = "ELO Anything!"
    , body =
        [ Html.main_ []
            [ rankings (Dict.values model.players)
            , newPlayerForm model
            ]
            |> Html.toUnstyled
        ]
    }


rankings : List Player -> Html msg
rankings players =
    players
        |> List.sortBy .rating
        |> List.map
            (\player ->
                Html.tr
                    []
                    [ Html.td [] [ Html.text player.name ]
                    , Html.td [] [ Html.text (String.fromInt player.rating) ]
                    , Html.td [] [ Html.text (String.fromInt player.matches) ]
                    ]
            )
        |> (::)
            (Html.tr
                []
                [ Html.th [] [ Html.text "Name" ]
                , Html.th [] [ Html.text "Rating" ]
                , Html.th [] [ Html.text "Matches" ]
                ]
            )
        |> Html.table []


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
