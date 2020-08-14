module Main exposing (..)

import Browser exposing (Document)


type alias Flags =
    ()


type alias Model =
    {}


type alias Msg =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "ELO Anything!"
    , body = []
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
